package scala.reflect.persistence.sbt /* TODO: check for proper package */

import sbt._
import Keys._

class PersistencePlugin extends Plugin {
  println("hello")
 override lazy val settings = Seq(commands += myCommand)

  lazy val myCommand =
    Command.command("hello") { (state: State) =>
      println("Hi!")
      state
    }
}