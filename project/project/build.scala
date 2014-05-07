import sbt._
object PluginDef extends Build {
    lazy val root = project.in(file(".")).dependsOn( sbtPPlugin )
    lazy val sbtPPlugin = file("../sbt")
}