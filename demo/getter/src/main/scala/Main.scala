import scala.reflect.persistence._

object Main {
  val ru = scala.reflect.runtime.universe
  val toolbox = new ToolBox[ru.type](ru)
  val m = ru.runtimeMirror(getClass.getClassLoader)
  
  import ru._
  import toolbox._

  def main(args: Array[String]): Unit = {
    /* Can we fetch asts? */
	val symbol = m.staticModule("Basic").info.decl(TermName("foo")).asMethod
	println(symbol.associatedFile)
	println(symbol.pos.source)
    println(show(symbol.source))
  }
}