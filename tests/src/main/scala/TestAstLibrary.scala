import scala.reflect.persistence.ToolBox

object TestAstLibrary {
  val u = scala.reflect.runtime.universe
  import u._
  val toolbox = new ToolBox(u)
  import toolbox._ /* Should do the trick to import the implicit class */

  def main(args: Array[String]) {
    /* The resource should exist: let's try to fetch it. */
    assert(this.getClass.getResource("/Basic.scala.ast") != null, "The resource should exist in the classpath")
    
    val tt = getAst("Basic.scala.ast")
    println(tt)
  }
}