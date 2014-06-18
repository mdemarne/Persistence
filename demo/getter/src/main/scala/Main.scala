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
	/* For now, we use sourceFrom instead of just source, since the associated file is empty and need to be fixed */
    println(show(symbol.sourceFrom("Basic.scala.ast")))
  }
}