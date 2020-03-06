name := "sequentish"
organization := "net.rosien"
version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.10"

// scalaOptions provided by sbt-tpolecat plugin

libraryDependencies ++= Seq(
  "io.higherkindness" %% "droste-core" % "0.8.0",
  "org.typelevel" %% "cats-core" % "2.0.0"
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
