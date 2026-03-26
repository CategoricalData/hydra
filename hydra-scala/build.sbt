val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala",
    version := "0.1.0",

    scalaVersion := scala3Version,

    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "gen-main" / "scala",

    // Test sources include generated tests
    Test / unmanagedSourceDirectories += baseDirectory.value / "src" / "gen-test" / "scala",

    // Lib dependencies
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.12.0",

    // Fork JVM for run with increased stack
    Compile / run / fork := true,
    Compile / run / javaOptions ++= Seq("-Xss256m", "-Xmx4g"),

    // Fork JVM for tests with increased stack and memory
    Test / fork := true,
    Test / javaOptions ++= Seq("-Xss256m", "-Xmx4g"),

    // Testing dependencies
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
