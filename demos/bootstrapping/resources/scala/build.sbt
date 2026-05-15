// Standalone build.sbt for bootstrapped Hydra Scala.
// Compiles the generated Scala under src/main/scala/ together with the
// hand-written runtime under src/main/scala/hydra/lib/. Self-contained:
// does NOT reference the parent repo's dist/.
val scala3Version = "3.3.7"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala-Bootstrap",
    version := "0.16.0",

    scalaVersion := scala3Version,

    libraryDependencies += "org.apache.commons" % "commons-text" % "1.12.0",

    Compile / run / fork := true,
    Compile / run / javaOptions ++= Seq("-Xss256m", "-Xmx4g"),

    Test / fork := true,
    Test / javaOptions ++= Seq("-Xss256m", "-Xmx4g"),

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"
  )
