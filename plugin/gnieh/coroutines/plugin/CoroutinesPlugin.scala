package gnieh.coroutines.plugin

import scala.tools.nsc
import scala.tools.nsc.typechecker._
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent

class CoroutinesPlugin(val global: Global) extends Plugin {
  import global._

  val name = "coroutines"
  val description = "applies coroutines conversion"
  
  val phase = new CoroutinesTransform() { 
    val global = CoroutinesPlugin.this.global
    val runsAfter = List("pickler")
    // it must run before continuations are transformed
    override val runsBefore = List("selectiveanf")
  }
  
  
  val components = List[PluginComponent](phase)

  global.log("instantiated coroutines plugin: " + this)

  // TODO: require -enabled command-line flag?
  
  override def processOptions(options: List[String], error: String => Unit) = {
    var enabled = false
    for (option <- options) {
      if (option == "enable") {
        enabled = true
      } else if(option == "verbose"){
        phase.verbose = true
      }else {
        error("Option not understood: "+option)
      }
    }
  }

  override val optionsHelp: Option[String] = 
    Some("  -P:coroutines:verbose        verbose mode")
    //Some("  -P:coroutines:enable        Enable coroutines (continuations must be enabled too)")
}
