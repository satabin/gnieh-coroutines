package gnieh.coroutines.plugin

import scala.collection._

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.plugins._
import scala.tools.nsc.symtab.Flags._

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.ast._

abstract class CoroutinesTransform extends PluginComponent with TypingTransformers with CoroutinesUtils {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._ // the global environment
  import definitions._ // standard classes and methods
  import typer.atOwner // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "coroutines"

  val coroutineName = "coroutine$"
  var coroutineCount = -1

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CoroutinesTransformer(unit)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTransformer(unit) transformUnit unit
    }
  }
  
  class CoroutinesTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    var owner: Symbol = NoSymbol
    var inCoroutine = false
    var hasYield = false

    protected def newCoroutineClass(pos: Position) = {
      coroutineCount += 1
      owner.newClass(pos, newTypeName(coroutineName + coroutineCount))
    }

    override def transform(tree: Tree): Tree = tree match {
      case _: ClassDef | _: ModuleDef | _: DefDef | _: ValDef =>
        val oldOwner = owner
        owner = tree.symbol
        val res = super.transform(tree)
        owner = oldOwner
        res
      case Apply(create, (fun: Function) :: Nil) if (create.symbol == MethCreate) =>
        inCoroutine = true

        // transform the call to the create method to an instantiation of the
        // Coroutine class
        localTyper.context1.reportGeneralErrors = true
        val result = atPhase(currentRun.phaseNamed("typer")) {
          localTyper.typed(
            transformCreate(tree.symbol, fun))
        }

        inCoroutine = false
        result
      case Apply(create, (fun: Function) :: Nil) if (create.symbol == MethWrap) =>
        inCoroutine = true

        val result = atPhase(currentRun.phaseNamed("typer")) {
          localTyper.typed(
            transformWrap(tree.symbol, fun))
        }

        inCoroutine = false
        result
      case Apply(create, (block: Tree) :: Nil) if (create.symbol == MethYld) =>
        if(!inCoroutine) {
          unit.error(tree.pos, "coroutines.yld must be used in a coroutine declaration")
          tree
        } else {
          transformYield(tree.symbol, block)
        }
      case _ => super.transform(tree)
    }

    /**
     * Transforms a call to the create method to the corresponding instantiation
     * <pre>
     * coroutines.create {
     *   (p: Param) => block
     * }
     * </pre>
     * is transformed to
     * <pre>
     * new Coroutine[Param: Ret] {
     *   protected var fun = (p: Param) => reset { block }
     * }
     * </pre>
     */
    def transformCreate(create: Symbol, fun: Function): Tree = {

      val (paramType, retType) = fun.tpe match {
        //case MethodType(p :: Nil, ret) => (p, ret)
        case TypeRef(_, f, p :: ret :: Nil) if f == FunctionClass(1) =>
          (p, ret)
        case _ =>
          unit.error(fun.pos, "A function is expected. found: " + fun.tpe)
          (ErrorType, ErrorType)
      }

      // the anonymous class definition
      val newClass = owner.newAnonymousClass(create.pos) setFlag FINAL
        //newCoroutineClass(create.pos) setFlag (FINAL)

      val funSym = newClass.newVariable(fun.pos, "fun") setFlag (PRIVATE | LOCAL)
      funSym setInfo appliedType(FunctionClass(1).tpe, paramType :: retType :: Nil)

      val reset =
        Apply(
          TypeApply(
            Select(
              Select(
                Select(
                  Ident("scala"),
                  newTermName("util")),
                newTermName("continuations")),
              newTermName("reset")),
            Ident(retType.typeSymbol) :: Ident(retType.typeSymbol) :: Nil),
          transform(fun.body) :: Nil)

      // the type info
      newClass setInfo ClassInfoType(
        appliedType(Coroutine.tpe,
          paramType :: retType :: Nil) :: ScalaObjectClass.tpe :: Nil,
        new Scope, newClass)

      val getter = funSym.newGetter setFlag (ACCESSOR | OVERRIDE) resetFlag (PRIVATE | LOCAL)
      val setter = newClass.newMethod(fun.pos, nme.getterToSetter(getter.name)) setFlag (ACCESSOR | OVERRIDE) resetFlag (PRIVATE | LOCAL)

      setter.setInfo(
        MethodType(funSym.cloneSymbol(setter).setFlag(PARAM).resetFlag(MUTABLE | PRIVATE | LOCAL) :: Nil,
          UnitClass.tpe))
      newClass.info.decls enter funSym
      newClass.info.decls enter getter
      newClass.info.decls enter setter

      // the concrete members
      val funDef =
        ValDef(funSym, treeCopy.Function(fun, fun.vparams, localTyper.atOwner(newClass).typed(reset)))
      funDef.rhs.symbol.owner = funSym

      val getterDef = atOwner(newClass) {
        DefDef(getter, localTyper.typed(Select(This(newClass), funSym)))
      }
      val setterDef = atOwner(newClass) {
        DefDef(setter, localTyper.typed { Assign(Select(This(newClass), funSym), Ident(setter.info.params.head)) })
      }
      val body =
        funDef :: getterDef :: setterDef :: Nil

      val classDef =
        ClassDef(newClass, NoMods, List(List()), List(List()), body, fun.pos)

      // the anonymous class instantiation
      val instance = atPos(fun.pos) {
        Apply(Select(New(TypeTree(newClass.tpe)), nme.CONSTRUCTOR), Nil)
      }

      Block(classDef, instance)
    }

    /**
     * Transforms a call to the wrap method to the corresponding instantiation
     * <pre>
     * coroutines.wrap {
     *   (p: Param) => block
     * }
     * </pre>
     * is transformed to
     * <pre>
     * {
     *   val co = new Coroutine[Param: Ret] {
     *     protected var fun = (p: Param) => reset { block }
     *   }
     *   (p: Param) => co.resume(p)
     * }
     * </pre>
     */
    def transformWrap(wrap: Symbol, fun: Function): Tree = {
      // the associated coroutine
      val cor = transformCreate(wrap, fun)

      // the closure
      val corName = newTermName(coroutineName + coroutineCount)
      val cl = atPos(wrap.pos) {
        Function(ValDef(Modifiers(PARAM), "p", TypeTree(), EmptyTree) :: Nil,
          Apply(
            Select(
              Ident(corName),
              newTermName("resume")),
            Ident("p") :: Nil))
      }

      println(cl)

      val valCor = atPos(wrap.pos) {
        ValDef(NoMods, corName, TypeTree(), cor)
      }

      Block(valCor, cl)
    }

    /**
     * Transforms a call to the yield method to the corresponding shift block
     * <pre>
     * coroutines.yld(v)
     * </pre>
     * is transformed to
     * <pre>
     * shift { k: (Param => Ret) =>
     *   fun = k
     *   v
     * }
     * </pre>
     */
    def transformYield(yld: Symbol, arg: Tree): Tree = {
      EmptyTree
    }
    
    /**
     * Transforms a return (last instruction) to the corresponding shift block
     * <pre>
     * v
     * </pre>
     * is transformed to
     * <pre>
     * shift { k: (Unit => Unit) =>
     *   fun = consumed
     *   v
     * }
     * </pre>
     */
    def transformReturn(ret: Symbol, arg: Tree): Tree = {
      EmptyTree
    }

  }
}
