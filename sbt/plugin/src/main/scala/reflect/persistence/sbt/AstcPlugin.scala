package scala.reflect.persistence.sbt

import sbt._
import Keys._

import scala.language.existentials

object AstcPlugin extends Plugin {
  override lazy val projectSettings = Seq(packageAstTask, beforeCompileTask) ++ usePluginSettings ++ newCompile ++ publishSettings ++ packageSettings
  lazy val usePluginSettings = Seq(addCompilerPlugin("org.scalareflect" % "persistence-plugin_2.11.0" % "0.1.0-SNAPSHOT"))
  val packageAst = TaskKey[File]("package-ast", "Produce an artifact containing compressed Scala ASTs.")
  val packageAstTask = packageAst := {
    def findFiles(root: File): List[File] = root match {
      case _ if root.isDirectory => root.listFiles.toList.flatMap(f => findFiles(f))
      case _ => root :: Nil
    }
    val res = (compile in Compile).result.value match { /* First let's compile everything */
      case Value(analysis) =>
        val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
        val astsPath = generalPath + "/asts/"
        val outputJar = new File(generalPath + "/" + name.value + "_" + version.value + "-asts.jar")
        val astsSources = findFiles(new File(astsPath))
        val log = streams.value.log
        val manifest = new java.util.jar.Manifest()
        Package.makeJar(astsSources.map(f => (f, f.getAbsolutePath.replace(astsPath, ""))), outputJar, manifest, log)
        outputJar
      case Inc(inc: Incomplete) => /* Cannot package, since we get compilation error */
        throw inc /* Just re-throw exception */
    }
    res
  }
  /* Save the previously generated .ast files */
  val beforeCompile = TaskKey[Unit]("before-compile")
  val beforeCompileTask = beforeCompile := {
    val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
    (new File(generalPath + "/asts")).renameTo(new File(generalPath + "/asts.bak"))
  }
  /* If the compilation was successful, then we remove the old .asts. Otherwise we restore them. */
  val newCompile = compile in Compile := ({
    def deleteAll(f: File): Boolean = {
      if (f.isDirectory) f.listFiles.foreach(deleteAll(_))
      f.delete
    }
    beforeCompile.value /* Save the previous .asts (if any) */
    val generalPath = new File((classDirectory in Compile).value.getParent).getAbsolutePath
    var res = (compile in Compile).result.value match {
      case Inc(inc: Incomplete) => /* Restore the old .asts */
        (new File(generalPath + "/asts.bak")).renameTo(new File(generalPath + "/asts"))
        throw inc
      case Value(analysis) => /* If Compile was called without compiling anything, no new /asts folder will be generated */
        if (new File(generalPath + "/asts").exists) deleteAll(new File(generalPath + "/asts.bak"))
        else (new File(generalPath + "/asts.bak")).renameTo(new File(generalPath + "/asts"))
        analysis
    }
    res
  })
  /* Force publishing the ast artifact */
  lazy val publishSettings = (artifact in (Compile, packageAst) ~= { art =>
    art.copy(`classifier` = Some("asts"))
  }) ++ addArtifact(artifact in (Compile, packageAst), packageAst).settings
  /* In package, force also packaging the Asts */
  lazy val packageSettings = Keys.`package` in Compile := {
    (Keys.`package` in Compile).value
    (packageAst in Compile).value
  }
}