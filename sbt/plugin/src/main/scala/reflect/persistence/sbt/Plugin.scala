package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object PersistencePlugin extends Plugin {
  override lazy val projectSettings = Seq(commands += packageAst) ++ usePluginSettings

  lazy val packageAst =
    Command.command("PackageAst") { (state: State) =>
      /* TODO */
      state
    }

  lazy val plugin = Project(
    id   = "plugin",
    base = file("../../../../../../../../plugin")
  ) settings (
    assemblySettings: _*
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    libraryDependencies += "org.tukaani" % "xz" % "1.5",
    scalacOptions ++= Seq()
  ) settings (
    test in assembly := {}
  )

  lazy val usePluginSettings = Seq(
    scalacOptions in Compile <++= (AssemblyKeys.`assembly` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )
}