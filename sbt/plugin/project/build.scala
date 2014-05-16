import sbt._
import Keys._

object build extends Build {
  lazy val sbtPPlugin = Project(
    id = "sbt-persistence",
    base = file(".")
  ) settings (
  	sbtPlugin := true,
  	scalacOptions ++= Seq()
  ) dependsOn( assemblyPlugin )

  lazy val assemblyPlugin = uri("git://github.com/sbt/sbt-assembly")

}
