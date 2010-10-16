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

abstract class Coroutine[Param, Ret] {
  protected var fun: Param => Ret //@cpsParam[Ret,Ret]
  def resume(p: Param): Ret = {
    //	  reset(fun(p))
    fun(p)
  }
}

object Test {
  def main(args: Array[String]) {
    val co = new Coroutine.wrap[Unit, Int] {
      fun = (_: Unit) => {
        reset {
          var i = 1
          println("first time: " + i)
          //        yld(i+1)
          shift { k: (Unit => Int) =>
            fun = k
            i + 1
          }
          i += 1
          println("second time: " + i)
          shift { k: (Unit => Int) =>
            fun = k
            i += 7
            i
          }
          println("third time: " + i)
          i + 3
        }
      }
    }

    println(co())
    println("truie")
    println(co())
    println("gnieh")
    println(co())
  }
}

// vim: set ts=4 sw=4 et:
