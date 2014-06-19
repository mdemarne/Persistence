/**
 * Main component of the compiler plugin. Apply the whole compression piteline
 * and generate the good path for storing ASTs.
 *
 * @author Mathieu Demarne, Adrien Ghosn
 */
package scala.reflect.persistence

import scala.tools.nsc.{ Global, Phase, SubComponent }
import scala.tools.nsc.plugins.{ Plugin => NscPlugin, PluginComponent => NscPluginComponent }
import java.io.DataOutputStream
import java.io.FileOutputStream
import java.io.File

class Plugin(val global: Global) extends NscPlugin {
  import global._
  import Enrichments._

  val name = "persistence"
  val description = """Persists typed ASTs of the entire program.
  For more information visit https://github.com/scalareflect/persistence"""
  val components = List[NscPluginComponent](AstcPluginComponent)

  private object AstcPluginComponent extends NscPluginComponent {
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
        val compConstants = new ConstantCompressor[global.type](global)(decTree.constants.map(v => (v._1, v._2)))
        new XZWriter(new DataOutputStream(new FileOutputStream(path)))(compAsts ++ compNames ++ compConstants)
      }
    }

    /* generate a path for the compressed ASTs and names */
    def generatePath(unit: CompilationUnit): String = {
      val outputDir = (settings.outputDirs.getSingleOutput match {
        case None => sys.error("No output directory?") /* Should never happen */
        case Some(compilationDest) => compilationDest.container.toString
      }) + "/asts/" + (unit.body match {
        case p @ PackageDef(pid: Ident, stree) if p.name.toString != "<empty>"=> p.name.toString
        case p @ PackageDef(pid: Select, stree) if p.name.toString != "<empty>"=> (pid.qualifier.toString + "." + pid.name.toString).replaceAll("\\.", "/") + "/"
        case _ => "" /* No package at all */
      })
      val folder = new File(outputDir)
      if (!folder.exists()) folder.mkdirs()
      outputDir + unit.source.toString + ".ast"
    }
  }
}
