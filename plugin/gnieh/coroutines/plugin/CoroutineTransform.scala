/* This file is part of gnieh-coroutines.
*
* See the NOTICE file distributed with this work for copyright information.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
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
    var inCoroutine: Symbol = null
    var corparam: Type = null
    var corret: Type = null

    protected def newCoroutineClass(pos: Position) = {
      coroutineCount += 1
      owner.newClass(pos, newTypeName(coroutineName + coroutineCount))
    }

    override def transform(tree: Tree): Tree = try {
      tree match {
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
              atPos(create.pos) {
                transformCreate(tree.symbol, fun)
              })
          }
        case Apply(wrap, (fun: Function) :: Nil) if (wrap.symbol == MethWrap) =>
          atPhase(currentRun.phaseNamed("typer")) {
            localTyper.typed(
              atPos(wrap.pos) {
                transformWrap(tree.symbol, fun)
              })
          }
        case Apply(yld, (block: Tree) :: Nil) if (yld.symbol == MethYld) =>
          if (inCoroutine == null) {
            unit.error(tree.pos, "coroutines.yld must be used in a coroutine declaration")
            tree
          } else {
            atPos(yld.pos) {
              transformYield(block)
            }
          }
        case Apply(Apply(cowhile, cond :: Nil), body :: Nil) if (cowhile.symbol == MethCowhile) =>
          if (inCoroutine == null) {
            unit.error(tree.pos, "coroutines.cowhile must be used in a coroutine declaration")
            tree
          } else {
            atPos(cowhile.pos) {
              transformCowhile(tree.symbol, cond, body)
            }
          }
        case _ => super.transform(tree)
      }
    } catch {
      case t =>
        unit.error(tree.pos, t.getMessage)
        tree
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

      val (funBody, ret) = fun.body match {
        case Block(instr, ret) => (instr, ret)
        case _ => (Nil, fun.body)
      }

      // the type info
      newClass setInfo ClassInfoType(
        appliedType(Coroutine.tpe,
          corparam :: corret :: Nil) :: ScalaObjectClass.tpe :: Nil,
        new Scope, newClass)

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

      val funSetter = newClass.info.member(nme.getterToSetter("fun"))

      val newFun = treeCopy.Function(fun, fun.vparams, localTyper.typed(reset))
      newFun.symbol.owner = newClass

      val funDef =
        Apply(
          Select(
            This(newClass),
            funSetter.name),
          newFun :: Nil)

      val body =
        funDef :: Nil

      val classDef =
        ClassDef(newClass, NoMods, List(List()), List(List()), body, fun.pos)

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
        Apply(
          Select(
            This(inCoroutine),
            newTermName("yld")),
          transform(arg) :: Nil)
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
        atPos(i.pos) {
          If(transform(cond), transformedThen, transformedElse)
        }
      case _ =>
        localTyper.typed {
          Apply(
            Select(
              This(inCoroutine),
              newTermName("ret")),
            transform(arg) :: Nil)
        }
    }

    /**
     * Transforms a while statement:
     * <pre>
     * coroutines.cowhile(cond) {
     *   block
     * }
     * </pre>
     * is transformed to
     * <pre>
     * this.cowhile(cond) {
     *   block
     * }
     * </pre>  
     */
    def transformCowhile(cowhile: Symbol, cond: Tree, body: Tree): Tree = {

      val body1 = transform(body) match {
        case Block(stmts, last) =>
          Block(stmts ::: List(last), Literal(()))
        case b => b
      }
      localTyper.typed {
        Apply(
          Apply(
            Select(
              This(inCoroutine),
              newTermName("cowhile")),
            List(transform(cond))), List(body1))
      }
    }

  }
}
