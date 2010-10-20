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

import scala.tools.nsc.Global

trait CoroutinesUtils {
  val global: Global
  import global._
  import definitions._

  // TODO option?
  //var coroutinesEnabled = false
  var verbose: Boolean = false
  def vprintln(x: => Any): Unit = if (verbose) println(x)

  lazy val Coroutine = definitions.getClass("gnieh.coroutines.Coroutine")
  
  lazy val ModCoroutines = definitions.getModule("gnieh.coroutines")
  lazy val MethYld = definitions.getMember(ModCoroutines, "yld")
  lazy val MethCreate = definitions.getMember(ModCoroutines, "create")
  lazy val MethWrap = definitions.getMember(ModCoroutines, "wrap")
  
  lazy val ModContinuations = definitions.getModule("scala.util.continuations")
  lazy val MethShift = definitions.getMember(ModContinuations, "shift")
  lazy val MethReset = definitions.getMember(ModContinuations, "reset")
  
  def printlnr[T](o: T): T = {
    println(o)
    o
  }

}