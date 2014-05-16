import sbt._
import Keys._

// imports standard command parsing functionality
// see http://www.scala-sbt.org/release/docs/Extending/Commands.html
import complete.DefaultParsers._

object build extends Build {
  lazy val sbtPPlugin = Project(
    id = "sbt-persistence",
    base = file(".")
  ) settings (
  	scalaVersion := "2.11.0",
    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
  	sbtPlugin := true
  )
}
