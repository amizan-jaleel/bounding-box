ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "bounding-box",
    idePackagePrefix := Some("org.amizan"),

   libraryDependencies += "org.specs2" %% "specs2-core" % "4.21.0" % "test"
)
