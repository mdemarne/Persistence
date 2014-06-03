package scala.reflect.persistence

/* TODO: clean imports */
import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import scala.language.postfixOps
import scala.annotation.tailrec
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File
import org.tukaani.xz.LZMA2InputStream
import java.io.{ File, FileInputStream, FileOutputStream }
import org.tukaani.xz.{ XZOutputStream, LZMA2Options }

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import Enrichments._

  val name = "persistence"
  val description = """Persists typed ASTs of the entire program.
  For more information visit https://github.com/scalareflect/persistence"""
  val components = List[NscPluginComponent](PluginComponent) // Might change name

  private object PluginComponent extends NscPluginComponent {
    import global._
    val global = Plugin.this.global

    override val runsAfter = List("typer")
    val phaseName = "persist"

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        val path = generatePath(unit)
        val decTree = new TreeDecomposer[global.type](global)(unit.body)
        val compAsts = new AstCompressor()(decTree.treeBFS.toTree)
        val compNames = new NameCompressor()(decTree.names)
        new XZWriter(new DataOutputStream(new FileOutputStream(path)))(compAsts ++ compNames)
      }
    }

    /* generate a path for the compressed ASTs and names */
    def generatePath(unit: CompilationUnit): String = {
      val outputDir = (settings.outputDirs.getSingleOutput match {
        case None => sys.error("No output directory?") /* Should never happen */
        case Some(compilationDest) => compilationDest.container.toString
      }) + "/asts/" + (unit.body match {
        case p: PackageDef if p.name.toString != "<empty>" => p.name.toString.replaceAll("\\.", "/") + "/"
        case _ => "" /* No package at all */
      })
      val folder = new File(outputDir)
      if (!folder.exists()) folder.mkdirs()
      outputDir + unit.source.toString + ".ast"
    }
  }
}
