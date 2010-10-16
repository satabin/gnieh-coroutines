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
  var coroutineCount = 0

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CoroutinesTransformer(unit)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTransformer(unit) transformUnit unit
    }
  }

  class CoroutinesTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    var inCoroutine = false

    override def transform(tree: Tree): Tree = tree match {
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
      val newClass = create.newAnonymousClass(create.pos)

      val funSym = newClass.newVariable(fun.pos, "fun") setFlag (OVERRIDE)
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

      val body =
        ValDef(funSym, treeCopy.Function(fun, fun.vparams, localTyper.typed(reset))) :: Nil
      val parent = AppliedTypeTree(Ident(Coroutine),
        Ident(paramType.typeSymbol) :: Ident(retType.typeSymbol) :: Nil)

      newClass setInfo ClassInfoType(
        appliedType(Coroutine.tpe,
          paramType :: retType :: Nil) :: Nil,
        new Scope, newClass)

      newClass.info.decls enter funSym

      val getter = funSym.newGetter
      val setter = newClass.newMethod(fun.pos, nme.getterToSetter(getter.name)) setFlag (ACCESSOR)
      setter.setInfo(
        MethodType(funSym.cloneSymbol(setter).setFlag(PARAM).resetFlag(MUTABLE | OVERRIDE) :: Nil,
          UnitClass.tpe))
      newClass.info.decls enter getter
      newClass.info.decls enter setter

      val classDef =
        ClassDef(newClass, NoMods, List(List()), List(List()), body, fun.pos)

      println(classDef)

      // TODO the anonymous class instantiation

      Block(classDef)
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
    def transformedYield() {

    }

  }
}
