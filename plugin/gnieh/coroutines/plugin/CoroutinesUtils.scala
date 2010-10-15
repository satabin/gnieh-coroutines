package gnieh.coroutines.plugin

import scala.tools.nsc.Global

trait CoroutinesUtils {
  val global: Global
  import global._
  import definitions._

  // TODO option?
  //var coroutinesEnabled = false
  var verbose: Boolean = false
  def vprintln(x: => Any): Unit = if (verbose) println(x)

  lazy val Couroutine = definitions.getClass("gnieh.coroutines.Coroutine")
  
  lazy val ModCoroutines = definitions.getModule("gnieh.coroutines")
  lazy val MethYld = definitions.getMember(ModCoroutines, "yld")
  lazy val MethCreate = definitions.getMember(ModCoroutines, "create")
  lazy val MethWrap = definitions.getMember(ModCoroutines, "wrap")

}