package gnieh.coroutines.test

import gnieh.coroutines._
import scala.util.continuations._

object Test3 {

  /* original code
    val fib = coroutines.wrap {
    (_: Unit) => {
      coroutines.yld(1)
      coroutines.yld(1)
      var cur = 1
      var last = 1
      coroutines.cowhile(true) {
        cur = cur + last
        last = cur - last
        coroutines.yld(cur)
      }
      -1
    }
  }*/

  val fib = wrap(new Coroutine[Unit, Int] {
    fun = (_: Unit) => reset {
      yld(1)
      yld(1)
      var cur = 1
      var last = 1
      cowhile(true) {
        cur = cur + last
        last = cur - last
        yld(cur)
      }
      ret(-1)
    }
  })

  def main(args: Array[String]) {
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
    println(fib())
  }

}