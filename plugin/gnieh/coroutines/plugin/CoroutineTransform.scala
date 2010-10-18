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
      debug.traverse(unit.body)
    }
  }

  object debug extends Traverser {
    override def traverse(tree: Tree) = tree match {
      case t if t.hasSymbol && t.symbol == NoSymbol =>
        println(t + " =====> " + t.symbol)
        super.traverse(t)
      case _ => super.traverse(tree)
    }
  }

  class CoroutinesTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    var owner: Symbol = NoSymbol
    var inCoroutine: Symbol = null
    var corparam: Type = null
    var corret: Type = null

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

        // transform the call to the create method to an instantiation of the
        // Coroutine class
        atPhase(currentRun.phaseNamed("typer")) {
          localTyper.typed(
            transformCreate(tree.symbol, fun))
        }
      case Apply(wrap, (fun: Function) :: Nil) if (wrap.symbol == MethWrap) =>
        atPhase(currentRun.phaseNamed("typer")) {
          localTyper.typed(
            transformWrap(tree.symbol, fun))
        }
      case Apply(yld, (block: Tree) :: Nil) if (yld.symbol == MethYld) =>
        if (inCoroutine == null) {
          unit.error(tree.pos, "coroutines.yld must be used in a coroutine declaration")
          tree
        } else {
          transformYield(block)
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
    def transformCreate(create: Symbol, fun: Function): Block = {

      val oldCorparam = corparam
      val oldCorret = corret

      fun.tpe match {
        //case MethodType(p :: Nil, ret) => (p, ret)
        case TypeRef(_, f, p :: ret :: Nil) if f == FunctionClass(1) =>
          corparam = p
          corret = ret
        case _ =>
          unit.error(fun.pos, "A function is expected. found: " + fun.tpe)
          corparam = ErrorType
          corret = ErrorType
      }

      // the anonymous class definition
      val newClass = newCoroutineClass(create.pos) setFlag (FINAL)

      val oldCor = inCoroutine
      inCoroutine = newClass

      val funSym = newClass.newVariable(fun.pos, "fun") setFlag (PRIVATE | LOCAL)
      funSym setInfo appliedType(FunctionClass(1).tpe, corparam :: corret :: Nil)

      val (funBody, ret) = fun.body match {
        case Block(instr, ret) => (instr, ret)
        case _ => (Nil, fun.body)
      }

      // the type info
      newClass setInfo ClassInfoType(
        appliedType(Coroutine.tpe,
          corparam :: corret :: Nil) :: ScalaObjectClass.tpe :: Nil,
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
            Ident(UnitClass) :: Ident(corret.typeSymbol) :: Nil),
          Block(transformTrees(funBody) ::: transformReturn(ret.symbol, ret) :: Nil: _*) :: Nil)

      val funDef =
        ValDef(funSym, treeCopy.Function(fun, fun.vparams, localTyper.typed(reset)))
      funDef.rhs.symbol.owner = funSym
      new ForeachTreeTraverser(tree => {
        if (tree.isDef || tree.isInstanceOf[Function] && tree.symbol != NoSymbol)
          tree.symbol.owner = funDef.rhs.symbol
      }).traverse(reset)

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
        
        println(classDef)

      // the anonymous class instantiation
      val instance = atPos(fun.pos) {
        Apply(Select(New(TypeTree(newClass.tpe)), nme.CONSTRUCTOR), Nil)
      }

      inCoroutine = oldCor
      corparam = oldCorparam
      corret = oldCorret

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

      val oldCorparam = corparam
      val oldCorret = corret

      fun.tpe match {
        //case MethodType(p :: Nil, ret) => (p, ret)
        case TypeRef(_, f, p :: ret :: Nil) if f == FunctionClass(1) =>
          corparam = p
          corret = ret
        case _ =>
          unit.error(fun.pos, "A function is expected. found: " + fun.tpe)
          corparam = ErrorType
          corret = ErrorType
      }

      val corName = newTermName(coroutineName + (coroutineCount + 1))
      val corSym = owner.newValue(wrap.pos, corName)
      corSym.setInfo(appliedType(Coroutine.tpe, corparam :: corret :: Nil))
      val oldOwner = owner
      owner = corSym

      // the associated coroutine
      val cor = transformCreate(wrap, fun)

      owner = oldOwner

      // the closure

      val cl = atPos(wrap.pos) {
        Function(ValDef(Modifiers(PARAM), "p", TypeTree(), EmptyTree) :: Nil,
          Apply(
            Select(
              Ident(corSym),
              newTermName("resume")),
            Ident("p") :: Nil))
      }

      val valCor = atPos(wrap.pos) {
        ValDef(corSym, cor)
      }

      corparam = oldCorparam
      corret = oldCorret

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
    def transformYield(arg: Tree): Tree =
      localTyper.typed {
        createShift(corparam, corret, arg)
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
     * if the return value is an if
     * <pre>
     * if(cond) {
     *   ...
     *   v1
     * } else {
     *   ...
     *   v2
     * }
     * </pre>
     * it is transformed to
     * <pre>
     * if(cond) {
     *   ...
     *   shift { k: (Unit => Unit) =>
     *     fun = consumed
     *     v1
     *   }
     * } else {
     *   ...
     *   shift { k: (Unit => Unit) =>
     *     fun = consumed
     *     v2
     *   }
     * }
     * </pre>
     */
    def transformReturn(ret: Symbol, arg: Tree): Tree = arg match {
      case i@If(cond, thenp, elsep) =>
        def trans1(tree: Tree) = {
          tree match {
            case Block(stats, ret) =>
              Block(transformTrees(stats) ::: transformReturn(ret.symbol, ret) :: Nil: _*)
            case _ => transformReturn(ret, tree)
          }
        }
        val transformedThen = trans1(thenp)
        val transformedElse = trans1(elsep)
        localTyper.typed{
          atPos(i.pos){
            If(transform(cond), transformedThen, transformedElse)
          }
        }
      case _ =>
        localTyper.typed {
          createShift(corret, arg)
        }
    }

    private def createShift(kparam: Type, kret: Type, value: Tree): Apply =
      atPos(value.pos) {
        Apply(
          TypeApply(
            Select(
              Select(
                Select(
                  Ident("scala"),
                  newTermName("util")),
                newTermName("continuations")),
              newTermName("shift")),
            Ident(kparam.typeSymbol) :: Ident(kret.typeSymbol) :: Ident(kret.typeSymbol) :: Nil),
          Function(
            ValDef(Modifiers(PARAM), newTermName(inCoroutine.name + "$k"), TypeTree(), EmptyTree) :: Nil,
            Block(
              Apply(
                Ident(inCoroutine.info.member(nme.getterToSetter("fun"))),
                Ident(inCoroutine.name + "$k") :: Nil),
              transform(value))) :: Nil)
      }

    private def createShift(kret: Type, value: Tree): Apply =
      atPos(value.pos) {
        Apply(
          TypeApply(
            Select(
              Select(
                Select(
                  Ident("scala"),
                  newTermName("util")),
                newTermName("continuations")),
              newTermName("shift")),
            Ident(UnitClass) :: Ident(UnitClass) :: Ident(kret.typeSymbol) :: Nil),
          Function(
            ValDef(Modifiers(PARAM), newTermName(inCoroutine.name + "$k"), TypeTree(), EmptyTree) :: Nil,
            Block(
              Apply(
                Ident(inCoroutine.info.member(nme.getterToSetter("fun"))),
                Select(This(inCoroutine), newTermName("shot")) :: Nil),
              transform(value))) :: Nil)
      }

  }
}
