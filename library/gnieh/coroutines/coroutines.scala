package gnieh.coroutines

import scala.util.continuations._

class ConsumedCoroutineException(msg: String) extends Exception(msg)

abstract class Coroutine[Param, Ret] {

  protected object consumed extends Function1[Param, Ret] {
    def apply(p: Param): Ret = throw new ConsumedCoroutineException("this coroutine has already been consumed")
  }

  var fun: Param => Ret
  def resume(p: Param): Ret = {
    fun(p)
  }
}

// vim: set ts=4 sw=4 et:
