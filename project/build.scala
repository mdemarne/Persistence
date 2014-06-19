import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._
import scala.collection.mutable.ArrayBuffer

// imports standard command parsing functionality
// see http://www.scala-sbt.org/release/docs/Extending/Commands.html
import complete.DefaultParsers._

object build extends Build {
  lazy val sharedSettings = Defaults.defaultSettings ++ Seq(
    crossVersion := CrossVersion.full,
    version := "0.1.0-SNAPSHOT",
    organization := "org.scalareflect",
    description := "AST persistence for Project Palladium",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    resolvers += Resolver.sonatypeRepo("releases"),
    publishMavenStyle := true,
    publishArtifact in Compile := false,
    publishArtifact in Test := false,
    scalacOptions ++= Seq("-deprecation", "-feature", "-optimise"),
    parallelExecution in Test := false, // hello, reflection sync!!
    logBuffered := false,
    scalaHome := {
      val scalaHome = System.getProperty("persistence.scala.home")
      if (scalaHome != null) {
        println(s"Going for custom scala home at $scalaHome")
        Some(file(scalaHome))
      } else None
    }
    // TODO: how do I make this recursion work?
    // run <<= run in Compile in sandbox,
    // test <<= test in Test in tests
  )

  // http://stackoverflow.com/questions/20665007/how-to-publish-only-when-on-master-branch-under-travis-and-sbt-0-13
  val publishOnlyWhenOnMaster = taskKey[Unit]("publish task for Travis (don't publish when building pull requests, only publish when the build is triggered by merge into master)")
  def publishOnlyWhenOnMasterImpl = Def.taskDyn {
    import scala.util.Try
    val travis   = Try(sys.env("TRAVIS")).getOrElse("false") == "true"
    val pr       = Try(sys.env("TRAVIS_PULL_REQUEST")).getOrElse("false") != "false"
    val branch   = Try(sys.env("TRAVIS_BRANCH")).getOrElse("??")
    val snapshot = version.value.trim.endsWith("SNAPSHOT")
    (travis, pr, branch, snapshot) match {
      case (true, false, "master", true) => publish
      case _                             => Def.task ()
    }
  }
  lazy val publishableSettings = sharedSettings ++ Seq(
    publishMavenStyle := true,
    publishArtifact in Compile := true,
    publishOnlyWhenOnMaster := publishOnlyWhenOnMasterImpl.value,
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases" at nexus + "service/local/staging/deploy/maven2")
    },
    pomIncludeRepository := { x => false },
    pomExtra := (
      <url>https://github.com/scalareflect/persistence</url>
      <inceptionYear>2014</inceptionYear>
      <licenses>
        <license>
          <name>BSD-like</name>
          <url>http://www.scala-lang.org/downloads/license.html</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git://github.com/scalareflect/persistence.git</url>
        <connection>scm:git:git://github.com/scalareflect/persistence.git</connection>
      </scm>
      <issueManagement>
        <system>GitHub</system>
        <url>https://github.com/scalareflect/persistence/issues</url>
      </issueManagement>
    ),
    credentials ++= loadCredentials().toList
  )

  def loadCredentials(): Option[Credentials] = {
    val mavenSettingsFile = System.getProperty("maven.settings.file")
    if (mavenSettingsFile != null) {
      println("Loading Sonatype credentials from " + mavenSettingsFile)
      try {
        import scala.xml._
        val settings = XML.loadFile(mavenSettingsFile)
        def readServerConfig(key: String) = (settings \\ "settings" \\ "servers" \\ "server" \\ key).head.text
        Some(Credentials(
          "Sonatype Nexus Repository Manager",
          "oss.sonatype.org",
          readServerConfig("username"),
          readServerConfig("password")
        ))
      } catch {
        case ex: Exception =>
          println("Failed to load Maven settings from " + mavenSettingsFile + ": " + ex)
          None
      }
    } else {
      for {
        realm <- sys.env.get("SCALAREFLECT_MAVEN_REALM")
        domain <- sys.env.get("SCALAREFLECT_MAVEN_DOMAIN")
        user <- sys.env.get("SCALAREFLECT_MAVEN_USER")
        password <- sys.env.get("SCALAREFLECT_MAVEN_PASSWORD")
      } yield {
        println("Loading Sonatype credentials from environment variables")
        Credentials(realm, domain, user, password)
      }
    }
  }

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

  lazy val useShowRawPluginSettings = Seq(
    scalacOptions in Compile <++= (Keys.`package` in (sandbox, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      // Thanks Jason for this cool idea (taken from https://github.com/retronym/boxer)
      // add plugin timestamp to compiler options to trigger recompile of
      // main after editing the plugin. (Otherwise a 'clean' is needed.)
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    }
  )

  lazy val root = plugin.aggregate(library, sbtPPlugin)

  lazy val plugin = Project(
    id   = "persistence-plugin",
    base = file("plugin")
  ) settings (
    publishableSettings ++ assemblySettings: _*
  ) settings (
    scalaVersion := "2.11.0",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    libraryDependencies += "org.tukaani" % "xz" % "1.5",
    scalacOptions ++= Seq()
  ) settings (
    test in assembly := {}
  ) settings (
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some(""))
    }
  ) settings (
    addArtifact(artifact in (Compile, assembly), assembly).settings: _*
  ) settings (
    publishArtifact in (Compile, packageBin) := false
  )

  lazy val library = Project(
    id   = "persistence-library",
    base = file("library")
  ) settings (
    publishableSettings ++ assemblySettings: _*
  ) settings (
    scalaVersion := "2.11.0",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.scalatest" %% "scalatest" % "2.1.3" % "test",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.3" % "test",
    libraryDependencies += "org.tukaani" % "xz" % "1.5",
    libraryDependencies+= "org.apache.ant" % "ant-apache-bcel" % "1.8.4",
    scalacOptions ++= Seq()
  ) settings (
    test in assembly := {}
  ) settings (
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some(""))
    }
  ) settings (
    addArtifact(artifact in (Compile, assembly), assembly).settings: _*
  ) settings (
    publishArtifact in (Compile, packageBin) := false
  ) dependsOn(plugin % "test->test;compile->compile")

  lazy val sbtPPlugin = Project(
    id = "persistence-sbt",
    base = file("./sbt/plugin")
  ) settings (
      publishableSettings:_*
  ) settings (
    scalaVersion := "2.10.3", // Forced for now due to SBT macro previous compilation.
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
    sbtPlugin := true
  )

  lazy val sandbox = Project(
    id   = "sandbox",
    base = file("sandbox")
  ) settings (
    sharedSettings: _*
  ) settings (
    scalaVersion := "2.11.0",
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    libraryDependencies += "org.tukaani" % "xz" % "1.5",
     scalacOptions ++= Seq()
  )

  lazy val tests = Project(
    id   = "tests",
    base = file("tests")
  ) configs ( testScalalib, testScalalibNoPlug, testTypers, testTypersNoPlug, testBasic, testBasicNoPlug, testToolbox
  /*) configs ( testSpecificList ++ testSpecificNoPlugList:_**/
  ) settings (
    sharedSettings ++ useShowRawPluginSettings ++ usePluginSettings ++
    testScalalibConf ++ testScalalibConfNoPlug ++
    testTypersConf ++ testTypersConfNoPlug ++
    testBasicConf ++ testBasicConfNoPlug ++ testToolboxConf /*++ testSpecificConfList ++ testSpecificConfNoPlugList*/: _*
  ) settings (
    scalaVersion := "2.11.0",
    sources in Compile <<= (sources in Compile).map(_ filter(f => f.getAbsolutePath.contains(testAstLibraryFile)))
  ) settings (
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
    libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-compiler" % _),
    /* Forcing dependencies on ASTs publish from our SBT test, to test the decompression library */
    /* libraryDependencies += "default" % "tests_2.11" % "0.1-SNAPSHOT" classifier "asts", //TODO: uncomment for tests of the Toolbox */
    scalacOptions ++= Seq()
  ) dependsOn (library % "test->test;compile->compile")

  /* Custom configurations for compilation tests */

  lazy val testPluginConf = Seq(
    unmanagedSourceDirectories := (unmanagedSourceDirectories in Compile).value,
    classDirectory := (classDirectory in Compile).value,
    dependencyClasspath := (dependencyClasspath in Compile).value,
    /* Addition of both plugins. Since the configuration isn't compile, we cannot use the settings defined above */
    scalacOptions <++= (AssemblyKeys.`assembly` in (plugin, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    },
    scalacOptions <++= (Keys.`package` in (sandbox, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    })
  lazy val testNoPlugConf = Seq(
    unmanagedSourceDirectories := (unmanagedSourceDirectories in Compile).value,
    classDirectory := (classDirectory in Compile).value,
    dependencyClasspath := (dependencyClasspath in Compile).value,
    /* Addition of the showRaw plugin for consitency in time compilation */
    scalacOptions <++= (Keys.`package` in (sandbox, Compile)) map { (jar: File) =>
      val addPlugin = "-Xplugin:" + jar.getAbsolutePath
      val dummy = "-Jdummy=" + jar.lastModified
      Seq(addPlugin, dummy)
    })

  lazy val testScalalib = config("testScalalib") 
  lazy val testScalalibNoPlug = config("testScalalibNoPlug") 
  lazy val testTypers = config("testTypers")
  lazy val testTypersNoPlug = config("testTypersNoPlug")
  lazy val testBasic = config("testBasic")
  lazy val testBasicNoPlug = config("testBasicNoPlug")

  /* Config to use artifact packaged and published */
  lazy val testToolbox = config("testToolbox")

  val testAstLibraryFile = "TestAstLibrary.scala"
  lazy val testToolboxConf: Seq[Setting[_]] = inConfig(testToolbox)(Defaults.configSettings ++ testPluginConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => f.getAbsolutePath.contains(testAstLibraryFile))}  
  ))

  /* Generating specific configs for compiling file by file */
  /*val nbSources: Int = findFiles(new File("tests/src/")).size
  lazy val testSpecificList = for(i <- 0 until nbSources) yield(config(s"testSpecific${i}"))
  lazy val testSpecificNoPlugList = for(i <- 0 until nbSources) yield(config(s"testSpecificNoPlug${i}"))*/

  lazy val testScalalibConf: Seq[Setting[_]] = inConfig(testScalalib)(Defaults.configSettings ++ testPluginConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => f.getAbsolutePath.contains("scalalibrary/") && f.name != testAstLibraryFile)}
  ))
  lazy val testScalalibConfNoPlug: Seq[Setting[_]] = inConfig(testScalalibNoPlug)(Defaults.configSettings ++ testNoPlugConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => f.getAbsolutePath.contains("scalalibrary/") && f.name != testAstLibraryFile)}
  ))
  lazy val testTypersConf: Seq[Setting[_]] = inConfig(testTypers)(Defaults.configSettings ++ testPluginConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => f.name == "Typers.scala" && f.name != testAstLibraryFile)}
  ))
  lazy val testTypersConfNoPlug: Seq[Setting[_]] = inConfig(testTypersNoPlug)(Defaults.configSettings ++ testNoPlugConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => f.name == "Typers.scala" && f.name != testAstLibraryFile)}
  ))
  lazy val testBasicConf: Seq[Setting[_]] = inConfig(testBasic)(Defaults.configSettings ++ testPluginConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => !f.getAbsolutePath.contains("scalalibrary/") && f.name != "Typers.scala" && f.name != testAstLibraryFile)}
  ))
  lazy val testBasicConfNoPlug: Seq[Setting[_]] = inConfig(testBasicNoPlug)(Defaults.configSettings ++ testNoPlugConf ++ Seq(
    unmanagedSources := {unmanagedSources.value.filter(f => !f.getAbsolutePath.contains("scalalibrary/") && f.name != "Typers.scala" && f.name != testAstLibraryFile)}
  ))

  /* Generating specific settings for compiling file by file */
  /*var sourcePlug: Int = -1
  lazy val testSpecificConfList = testSpecificList.flatMap(conf => {
      val src = sourcePlug; sourcePlug+=1
      inConfig(conf)(Defaults.configSettings ++ packageAstTask ++ testPluginConf ++ Seq(
        unmanagedSources := {new ArrayBuffer += unmanagedSources.value(src)}
    ))
  })
  var sourceNoPlug: Int = -1
  lazy val testSpecificConfNoPlugList = testSpecificNoPlugList.flatMap(conf => {
      val src = sourcePlug; sourcePlug+=1
      inConfig(conf)(Defaults.configSettings ++ testNoPlugConf ++ Seq(
        unmanagedSources := {new ArrayBuffer += unmanagedSources.value(src)}
    ))
  })*/
}
