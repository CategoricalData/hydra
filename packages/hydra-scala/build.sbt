val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala",
    version := "0.16.0",

    scalaVersion := scala3Version,

    Compile / unmanagedSourceDirectories ++= Seq(
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-kernel" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-haskell" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-java" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-python" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-scala" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-lisp" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "heads" / "scala" / "src" / "main" / "scala",
    ),

    // Test sources include generated tests
    Test / unmanagedSourceDirectories ++= Seq(
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-kernel" / "src" / "test" / "scala",
      baseDirectory.value / ".." / ".." / "heads" / "scala" / "src" / "test" / "scala",
    ),

    // Exclude generation tests (Scala coder output tests) which require Array[Byte] binary support
    Test / unmanagedSources / excludeFilter := HiddenFileFilter || new SimpleFileFilter(
      _.getAbsolutePath.contains("/generation/")),

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
