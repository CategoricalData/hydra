val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Hydra-Scala",
    version := "0.1.0",

    scalaVersion := scala3Version,

    Compile / unmanagedSourceDirectories += baseDirectory.value / "src" / "gen-main" / "scala",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    libraryDependencies += "org.apache.commons" % "commons-text" % "1.9"
  )
