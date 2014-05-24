package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._


object PersistencePlugin extends Plugin {
  override lazy val projectSettings = Seq(commands += packageAst) ++ usePluginSettings

  lazy val packageAst =
    Command.command("PackageAst") { (state: State) =>
      /* TODO */
      state
    }

  /* Get the path to the plugin jar (should be generated prior to the test) */
  val astcJar = new File("../../plugin/target/scala-2.11/plugin-assembly-0.1.0-SNAPSHOT.jar")

  lazy val usePluginSettings = Seq( // TODO: addCompilerPlugin instead, command already made for that purpose
      scalacOptions ++= Seq("-Xplugin:" + astcJar.getAbsolutePath)
  )
}