package main.scala

import scala.reflect.persistence._

object Main {
  val u = scala.reflect.runtime.universe
  val toolbox = new ToolBox(u)
  import u._
  import toolbox._

  def main(args: Array[String]): Unit = {
	  /* Can we fetch asts? */
	  println(show(getMethodDef("Basic.scala.ast", "foo" :: Nil)))
  }

}