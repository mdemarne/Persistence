

import scala.tools.nsc.{Global, Phase}
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import java.io.DataOutputStream
import java.io.File
import java.io.FileOutputStream
import scala.tools.nsc.plugins.{Plugin => NscPlugin}
import scala.tools.nsc.plugins.{PluginComponent => NscPluginComponent}
import scala.tools.nsc.plugins.{Plugin => NscPlugin}
import scala.tools.nsc.plugins.{PluginComponent => NscPluginComponent}

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

        val folder = new File("showraw")
        if (!folder.exists()) folder.mkdir()

        /* TODO: take proper path into account (packages, etc.) */
        val output = new DataOutputStream(new FileOutputStream(s"showraw/${unit.source.toString}.raw"))
        output.writeChars(showRaw(unit.body))
      }
    }

  }
}