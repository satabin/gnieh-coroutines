package gnieh.coroutines

import scala.util.continuations._

class ShotCoroutineException(msg: String) extends Exception(msg)

abstract class Coroutine[Param, Ret] {

  object shot extends Function1[Param, Ret] {
    def apply(p: Param): Ret = throw new ShotCoroutineException("this coroutine has already been shot")
  }

  var fun: Param => Ret = shot
  
  def resume(p: Param): Ret = {
    fun(p)
  }
  
  def yld(v: Ret) = {
    shift {k: (Param => Ret) =>
      fun = k
      v
    }
  }
  
  def ret(v: Ret) = {
    shift {k: (Unit => Unit) =>
      fun = shot
      v
    }
  }
  
}

// vim: set ts=4 sw=4 et:
