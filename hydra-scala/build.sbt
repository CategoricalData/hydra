import sbt.Keys.libraryDependencies

val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala",
    version := "0.1.0",

    scalaVersion := scala3Version,

    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "gen-main" / "scala",

    // Lib dependencies
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.9",

    // Testing dependencies
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test",

    // JUnit can possibly be removed
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"
  )
