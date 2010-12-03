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
package gnieh.coroutines

import scala.util.continuations._

class corourinte[Param, Ret] extends StaticAnnotation

class ShotCoroutineException(msg: String) extends Exception(msg)

abstract class Coroutine[Param, Ret] {

  implicit val current = this

  object shot extends Function1[Param, Ret] {
    def apply(p: Param): Ret = throw new ShotCoroutineException("this coroutine has already been shot")
  }

  var fun: Param => Ret = shot

  def resume(p: Param): Ret = {
    fun(p)
  }

  def yld(v: Ret) = {
    shift { k: (Param => Ret) =>
      fun = k
      v
    }
  }

  def ret(v: Ret) = {
    shift { k: (Unit => Unit) =>
      fun = shot
      v
    }
  }

  def cpsunit: Unit @cps[Unit] = ()

  /** this allows use of yield from within a while loop
   * see http://stackoverflow.com/questions/2201882/implementing-yield-yield-return-using-scala-continuations/2218589#2218589
   */
  def cowhile(cond: => Boolean)(body: =>Param @cpsParam[Ret, Ret]):
    Unit @cpsParam[Ret, Ret] = {
    if (cond) {
      body
      cowhile(cond)(body)
    }
  }

}

// vim: set ts=4 sw=4 et:
