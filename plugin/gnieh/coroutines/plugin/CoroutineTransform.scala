package gnieh.coroutines.plugin

import scala.collection._

import scala.tools.nsc._
import scala.tools.nsc.transform._
import scala.tools.nsc.plugins._

import scala.tools.nsc.ast.TreeBrowsers
import scala.tools.nsc.ast._

abstract class CoroutinesTransform extends PluginComponent with 
                                  TypingTransformers with CoroutinesUtils {
  // inherits abstract value `global' and class `Phase' from Transform

  import global._                  // the global environment
  import definitions._             // standard classes and methods
  import typer.atOwner             // methods to type trees

  /** the following two members override abstract members in Transform */
  val phaseName: String = "coroutines"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new CoroutinesTransformer(unit)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      newTransformer(unit) transformUnit unit
    }
  }
  
  class CoroutinesTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {

    override def transform(tree: Tree): Tree = {
      super.transform(tree)
    }
    
    /**
     * Transforms a call to the yield function to the corresponding shift block
     * <pre>
     * 
     * </pre>
     */
    def transformYield() {
      
    }

  }
}
