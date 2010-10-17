package gnieh.coroutines

import scala.util.continuations._

object Coroutine {
  abstract class create[P, R] extends Coroutine[P, R]

  abstract class wrap[P, R] extends Function1[P, R] { self =>
    private[this] var _fun: P => R /*@cpsParam[R,R]*/ = null
    def fun = _fun
    def fun_=(f: P => R /*@cpsParam[R,R]*/ ) {
      _fun = f
      c.fun = f
    }
    lazy val c = new create[P, R] { var fun = self.fun }
    def apply(p: P): R = {
      /* resume coroutine */
      c.resume(p)
    }
  }

}

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
