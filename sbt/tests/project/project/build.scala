import sbt._
object build extends Build {
	/* TODO: when published, add sbt plugin via command 
	 * addSbtPlugin(...). For now it is hardcoded, since we use it locally only. */
    lazy val root = project.in(file(".")).dependsOn( sbtPPlugin )
    lazy val sbtPPlugin = file("../../plugin")
}