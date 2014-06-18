import scala.reflect.persistence.ToolBox

/**
  * TODO: This object will fail unless the sbt/tests/ project is published locally and the corresponding library dependency added to the build,
  * see line 207 of the tests project definition in the root project build (project/build.scala).
  */
object TestAstLibrary {
  val u = scala.reflect.runtime.universe
  import u._
  val toolbox = new ToolBox(u)
  import toolbox._ /* Should do the trick to import the implicit class */

  def main(args: Array[String]) {
    /* The resource should exist: let's try to fetch it. */
    assert(this.getClass.getResource("/Basic.scala.ast") != null, "The resource should exist in the classpath")
    
    val file = "Basic.scala.ast"
    
    println("test1:")
    val names = "Basic.test1".split('.').toList
    val dec = getMethodDef(file, names)
    println(dec)

    println("test2:")
    val names2 = "Basic.test2".split('.').toList
    val dec2 = getMethodDef(file, names2)
    println(dec2)

  }
}
