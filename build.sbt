val dottyVersion = "0.26.0-RC1"
val scala213Version = "2.13.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "the-hutton-in-dotty",
    version := "0.1.0",

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "1.0.1",
//      "org.typelevel" %% "cats-core" % "2.1.1",
      "com.novocode" % "junit-interface" % "0.11" % "test",
    ),

    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala213Version)
  )
