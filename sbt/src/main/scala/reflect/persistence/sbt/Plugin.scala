package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._

object PersistencePlugin extends Plugin {
  override lazy val projectSettings = Seq(commands += myCommand)

  lazy val myCommand =
    Command.command("hello") { (state: State) =>
      println("Hi!")
      state
    }
}