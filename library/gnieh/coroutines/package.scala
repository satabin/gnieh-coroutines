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
package gnieh

import scala.util._
import continuations._

package object coroutines {
  def fun[Param, Ret](p: Param): Ret = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def yld[T, V](v: T): V = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def create[Param, Ret](fun: Param => Ret): Coroutine[Param, Ret] = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def wrap[Param, Ret](fun: Param => Ret): Param => Ret = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  
  def wrap[Param,Ret](cor: Coroutine[Param,Ret]) = (p: Param) => cor.resume(p)
 
  /** this allows use of yield from within a while loop
   * see http://stackoverflow.com/questions/2201882/implementing-yield-yield-return-using-scala-continuations/2218589#2218589
   */
  def cowhile[Ret](cond: Boolean)(body: =>Ret): Ret = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  
}

// vim: set ts=4 sw=4 et:
