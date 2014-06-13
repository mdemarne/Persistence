import scala.reflect.persistence.ToolBox

object TestAstLibrary {
  val u = scala.reflect.runtime.universe
  import u._
  val toolbox = new ToolBox(u)
  import toolbox._ /* Should do the trick to import the implicit class */

  def main(args: Array[String]) {
    /* The resource should exist: let's try to fetch it. */
    assert(this.getClass.getResource("/Basic.scala.ast") != null, "The resource should exist in the classpath")
    
    val decTree = getAst("Basic2.scala.ast")
    println(decTree)
    
    println("\nJust parts\n")
    val names = "Basic2.test2".split('.').toList
    val dec = getMethodDef("Basic2.scala.ast", names)
    println(dec)
  }
}
