scalaOrganization in ThisBuild := "org.typelevel"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.2",
  "org.typelevel" %% "cats" % "0.8.0",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

scalaVersion := "2.11.8"

scalacOptions += "-Ypartial-unification"