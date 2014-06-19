import scala.reflect.persistence._

object Main {
  val ru = scala.reflect.runtime.universe
  val toolbox = new ToolBox[ru.type](ru)
  val m = ru.runtimeMirror(getClass.getClassLoader)

  import ru._
  import toolbox._

  def main(args: Array[String]): Unit = {

    /* Question: Can we fetch asts? */

    val symbol = m.staticModule("Basic").info.decl(TermName("foo")).asMethod
    
    /* Answer: Yes...? */

    println("Tree:")
    println(show(symbol.source))
    println("Raw:")
    println(showRaw(symbol.source))
  }
}
