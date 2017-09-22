import sbt._
import Keys._


object BuildSettings {
  val buildScalaVersion = "2.11.11"

  val buildSettings = Seq(
    organization       := "com.github.julien-truffaut",
    scalaVersion       := buildScalaVersion,
    scalacOptions     ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:implicitConversions", "-language:higherKinds", "-language:postfixOps",
      "-optimise",
      "-unchecked",
      "-Yno-generic-signatures",
      "-Yno-adapted-args",
      "-Yinline", "-Yinline-warnings",
      "-Ywarn-value-discard"
    ),
    resolvers          += Resolver.sonatypeRepo("releases"),
    resolvers          += Resolver.sonatypeRepo("snapshots"),
    resolvers          += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    resolvers          += "bintray/non" at "http://dl.bintray.com/non/maven"
  )}

object Dependencies {
  val cats       = "org.typelevel"   %% "cats-core"  % "0.9.0"
  val scalaCheck = "org.scalacheck"  %% "scalacheck" % "1.13.4" % "test"
  val scalatest  = "org.scalatest"   %% "scalatest"  % "3.0.1"  % "test"
}

object MonocleBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "FpInScala",
    file("."),
    settings = buildSettings ++ Seq(
      publishArtifact := false
  )) aggregate(exercice)

  lazy val exercice: Project = Project(
    "exercice",
    file("exercice"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(cats, scalaCheck, scalatest)
    )
  )

}

