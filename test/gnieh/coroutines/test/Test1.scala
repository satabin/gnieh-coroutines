package gnieh.coroutines
package test

import scala.util.continuations._

// how the code looks like once transformed
object Test1 {
  def main(args: Array[String]) {
    
    /* original code:
       val co = coroutines.create {
         (_: Int) => {
           var i = 1
           println("first time: " + i)
           yld(i+1)
           i += 1

           println("second time: " + i)
           i += 7
           yld(i)
           println("third time: " + i)

           if (i < 0) {
             i+3
           } else {
             yld(i)
             i - 5
           }
         }
       }
     */
    
    val co = new Coroutine[Int, Int] {
      fun = (_: Int) => {
        reset {
          var i = 1
          println("first time: " + i)
          //        yld(i+1)
          shift { k: (Int => Int) =>
            fun = k
            i + 1
          }
          i += 1

          println("second time: " + i)
          i += 7
          //        yld(i)
          shift { k: (Int => Int) =>
            fun = k
            i
          }
          println("third time: " + i)

          val truie = if (i < 0) {
            //        i+3
            shift { k: (Unit => Unit) =>
              fun = shot
              i + 3
            }
          } else {
            //        yld(i)
            shift { k: (Int => Int) =>
              fun = k
              i
            }
            // i - 5
            shift { k: (Unit => Unit) =>
              fun = shot
              i - 5
            }
          }

        }
      }
    }

    println(co.resume(1))
    println("truie")
    println(co.resume(2))
    println("gnieh")
    println(co.resume(-6))
    println("gnieh2")
    println(co.resume(-9))
  }
}