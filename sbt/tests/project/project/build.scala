import sbt._
object build extends Build {
    lazy val root = project.in(file(".")).dependsOn( sbtPPlugin )
    lazy val sbtPPlugin = file("../../plugin")
}