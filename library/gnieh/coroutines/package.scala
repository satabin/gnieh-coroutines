package gnieh

import scala.util._
import continuations._

package object coroutines {
  def fun[Param,Ret](p: Param): Ret = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def yld[T,V](v: T): V = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def create[Param, Ret](fun: Param => Ret): Coroutine[Param, Ret] = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
  def wrap[Param, Ret](fun: Param => Ret): Param => Ret = throw new NoSuchMethodException("this code has to be compiled with the Scala coroutines plugin enabled")
}

// vim: set ts=4 sw=4 et:
