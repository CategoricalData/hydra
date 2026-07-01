val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala",
    version := "0.17.1",
    licenses := Seq("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")),

    scalaVersion := scala3Version,

    // The hand-written Scala runtime (hydra/lib/Libraries.scala, hydra/scala/lib/*)
    // and the test runner (hydra/TestSuiteRunner.scala, hydra/test/testEnv.scala)
    // now live in overlay/scala/hydra-kernel/ and are copied into the dist tree by
    // the assembler (#434), so they appear on the dist/scala/hydra-kernel/src paths
    // below. heads/scala/src/main/scala carries ONLY the generation drivers
    // (Bootstrap.scala, Generation.scala) now — no longer the runtime, so there is
    // no duplicate-class clash with the dist copy.
    Compile / unmanagedSourceDirectories ++= Seq(
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-kernel" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-haskell" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-jvm" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-java" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-python" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-scala" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-lisp" / "src" / "main" / "scala",
      baseDirectory.value / ".." / ".." / "heads" / "scala" / "src" / "main" / "scala",
    ),

    // Test sources: generated tests + the hand-written TestSuiteRunner/testEnv,
    // both under dist/scala/hydra-kernel/src/test/scala after the overlay copy.
    Test / unmanagedSourceDirectories ++= Seq(
      baseDirectory.value / ".." / ".." / "dist" / "scala" / "hydra-kernel" / "src" / "test" / "scala",
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
