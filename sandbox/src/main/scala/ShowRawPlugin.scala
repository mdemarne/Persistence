
import scala.tools.nsc.{ Global, Phase }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream
import scala.tools.nsc.plugins.{ Plugin => NscPlugin }
import scala.tools.nsc.plugins.{ PluginComponent => NscPluginComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin }
import scala.tools.nsc.plugins.{ PluginComponent => NscPluginComponent }
import java.io.PrintWriter
import java.util.Scanner

class ShowRawPlugin(val global: Global) extends NscPlugin {
  import global._

  val name = "ShowRawPlugin"
  val description = """Simply print the raw of a program"""
  val components = List[NscPluginComponent](PluginComponent)

  private object PluginComponent extends NscPluginComponent {
    import global._
    val global = ShowRawPlugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "showRaw"
    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        outputSource(unit)
        outputRaw(unit)
      }
      /* TODO: cleanup */
      def outputSource(unit: CompilationUnit) = {
        val outputDir = (settings.outputDirs.getSingleOutput match {
          case None => sys.error("No output directory?") /* TODO */
          case Some(compilationDest) => compilationDest.container.toString
        }) + "/sources/" + (unit.body match {
          case p: PackageDef if p.name.toString != "<empty>" => p.name.toString.replaceAll("\\.", "/") + "/"
          case _ => ""
        })
        val folder = new File(outputDir)
        if (!folder.exists()) folder.mkdirs()
        
        val path = outputDir + unit.source.toString + ".source"
        val input = new File(unit.source.file.path)
        val output = new File(path)
        val sc = new Scanner(input)
        val printer = new PrintWriter(output)
        while (sc.hasNextLine()) {
          printer.write(sc.nextLine() + "\n")
        }
        printer.flush()
      }
      
      def outputRaw(unit: CompilationUnit) = {
        val outputDir = (settings.outputDirs.getSingleOutput match {
          case None => sys.error("No output directory?") /* TODO */
          case Some(compilationDest) => compilationDest.container.toString
        }) + "/raw/" + (unit.body match {
          case p: PackageDef if p.name.toString != "<empty>" => p.name.toString.replaceAll("\\.", "/") + "/"
          case _ => ""
        })
        val folder = new File(outputDir)
        if (!folder.exists()) folder.mkdirs()
        
        val path = outputDir + unit.source.toString + ".raw"

        val output = new PrintWriter(path)
        output.write(showRaw(unit.body))
        output.flush()
      }
    }

  }
}