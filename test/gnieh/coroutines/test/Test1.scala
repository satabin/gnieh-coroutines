package gnieh.coroutines
package test

import scala.util.continuations._

// how the code looks like once transformed
object Test1 {
  def main(args: Array[String]) {
    val co = new Coroutine[Unit, Int] {
      var fun: Unit => Int = (_: Unit) => {
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
          i += 7
          //        yld(i)
          shift { k: (Unit => Int) =>
            fun = k
            i
          }
          println("third time: " + i)

          if (i < 0) {
            //        i+3
            shift { k: (Unit => Unit) =>
              fun = consumed
              i + 3
            }
          } else {
            //        yld(i)
            shift { k: (Unit => Int) =>
              fun = k
              i
            }
            // i - 5
            shift { k: (Unit => Unit) =>
              fun = consumed
              i - 5
            }
          }

        }
      }
    }

    println(co.resume(()))
    println("truie")
    println(co.resume(()))
    println("gnieh")
    println(co.resume(()))
    println("gnieh2")
    println(co.resume(()))
  }
}