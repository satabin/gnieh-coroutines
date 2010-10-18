Gnieh Coroutines
================

Introduction
------------

Gnieh Coroutines is a [Scala][1] library and a compiler plugin which makes it possible to use coroutines in Scala.
The design is based on coroutines in LUA ([Revisiting Coroutines][2], [Coroutines in LUA][3]) and natively implements asymmetric coroutines.

This project is still at an really early stage and is not finished yet. Any comments are welcome.

Usage
-----

There are two to create and use coroutines:
 *  coroutines.create: create and consume the coroutine object itself
 *  coroutines.wrap: create the coroutine and wrap it into a closure that takes the same parameter as the resume method

    import gnieh._
    val co = coroutines.create {
      (i: Int) => {
        println("first time: " + i)
        val j: Int = coroutines.yld(i + 5)
        println("second time: " + j)
        j - 7
      }
    }
    println(co.resume(2))
    println(co.resume(5))

is equivalent to

    import gnieh._
    val co = coroutines.wrap {
      (i: Int) => {
        println("first time: " + i)
        val j: Int = coroutines.yld(i + 5)
        println("second time: " + j)
        j - 7
      }
    }
    println(co(2))
    println(co(5))

Gnieh Coroutines is based on Scala continuations, so you need to enable the Scala continuations to compile the code above:
    scalac -Xplugin:/path/to/gnieh-coroutines-plugin.jar -cp /path/to/gnieh.coroutines-library.jar -P:continuations:enable cor.scala

Limitations
-----------

There are currently different limitations:
 *  coroutines.yld is only allowed in the function wrapped in the coroutine
 *  recursive calls to the function wrapped in the coroutine are not supported

Implementation
--------------

*TBD*

TODOs
-----

 *  Support for coroutines.yld in method not directly wrapped in the coroutine:
  *  add an implicit parameter to the method passing the current coroutine
 *  Support recursive calls?

[1]: http://www.scala-lang.org
[2]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.58.4017 "Revisiting Coroutines, Ana Lúcia de Moura and Roberto Ierusalimschy"
[3]: www.inf.puc-rio.br/~roberto/docs/corosblp.pdf "Coroutines in LUA, Ana Lúcia de Moura, Noemi Rodriguez, Roberto Ierusalimschy"
