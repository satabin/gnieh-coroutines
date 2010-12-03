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
    val runsAfter = List("typer")
    // it must run before continuations are transformed
//    override val runsBefore = List("selectiveanf")
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
