import sbt._
import Keys._

// imports standard command parsing functionality
// see http://www.scala-sbt.org/release/docs/Extending/Commands.html
import complete.DefaultParsers._

object build extends Build {
  lazy val sbtPPlugin = Project(
    id = "persistence-sbt",
    base = file(".")) settings (
      scalaVersion := "2.10.3", // Forced for now due to SBT macro system compilation.
      crossVersion := CrossVersion.full,
      version := "0.1.0-SNAPSHOT",
      organization := "org.scalareflect",
      description := "AST persistence for Project Palladium",
      resolvers += Resolver.sonatypeRepo("snapshots"),
      resolvers += Resolver.sonatypeRepo("releases"),
      scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
      // Thanks to https://github.com/gkossakowski/scala-sbt-cross-compile
      // add scala-xml dependency when needed (for Scala 2.11 and newer) in a robust way
      // this mechanism supports cross-version publishing
      libraryDependencies := {
        CrossVersion.partialVersion(scalaVersion.value) match {
          // if scala 2.11+ is used, add dependency on scala-xml module
          case Some((2, scalaMajor)) if scalaMajor >= 11 =>
            libraryDependencies.value ++ Seq(
              "org.scala-lang.modules" %% "scala-xml" % "1.0.1",
              "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
              "org.scala-lang.modules" %% "scala-swing" % "1.0.1")
          case _ =>
            // or just libraryDependencies.value if you don't depend on scala-swing
            libraryDependencies.value :+ "org.scala-lang" % "scala-swing" % scalaVersion.value
        }
      },
      sbtPlugin := true)
}
