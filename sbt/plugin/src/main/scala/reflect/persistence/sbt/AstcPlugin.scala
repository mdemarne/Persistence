package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._


object AstcPlugin extends Plugin {
  override lazy val projectSettings = Seq(packageAstTask) ++ usePluginSettings

  /* TODO: later, when the SBT plugin will be published, we will required to add it as 
   * addCompilerPlugin("..." %% "..." % _).
   * For now, let's just add it as a path option. */
  /* Get the path to the plugin jar (should be generated prior to the test) */
  val astcJar = new File("../../plugin/target/scala-2.11/plugin-assembly-0.1.0-SNAPSHOT.jar")
  lazy val usePluginSettings = Seq( // TODO: addCompilerPlugin instead, command already made for that purpose
      scalacOptions ++= Seq("-Xplugin:" + astcJar.getAbsolutePath)
  )

  /* TODO: check what we need as a Manifest(if relevant) */
  val packageAst = TaskKey[File]("package-ast", "Produce an artifact containing compressed Scala ASTs.")
  val packageAstTask = packageAst := {
    val generalPath = new File((fullClasspath in Compile).value.files.head.getParent).getAbsolutePath
    val astsPath = generalPath + "/asts/"
    val outputJar = new File(generalPath + "/" + name.value +"_" + version.value + "-asts.jar")
    val astsSources = findFiles(new File(astsPath))
    val log = streams.value.log
    val manifest = new java.util.jar.Manifest()
    Package.makeJar(astsSources.map(f => (f, f.getAbsolutePath.replace(astsPath, ""))), outputJar, manifest, log)
    outputJar
  }

  def findFiles(root: File): List[File] = root match {
      case _ if root.isDirectory => root.listFiles.toList.flatMap(f => findFiles(f))
      case _ => root :: Nil
  }
}