import sbt._
import Keys._


object BuildSettings {
  val buildScalaVersion = "2.11.6"

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
  val scalaz            = "org.scalaz"      %% "scalaz-core"               % "7.1.0"
  val scalaCheckBinding = "org.scalaz"      %% "scalaz-scalacheck-binding" % "7.1.0" % "test"
  val specs2Scalacheck  = "org.specs2"      %% "specs2-scalacheck"         % "2.4"   % "test"
  val scalazSpec2       = "org.typelevel"   %% "scalaz-specs2"             % "0.2"   % "test"
}

object MonocleBuild extends Build {
  import BuildSettings._
  import Dependencies._

  lazy val root: Project = Project(
    "FpInScala",
    file("."),
    settings = buildSettings ++ Seq(
      publishArtifact := false
  )) aggregate(exercice, response)

  lazy val exercice: Project = Project(
    "exercice",
    file("exercice"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(scalaz, scalaCheckBinding, specs2Scalacheck, scalazSpec2)
    )
  )

  lazy val response: Project = Project(
    "response",
    file("response"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(scalaz, scalaCheckBinding, specs2Scalacheck, scalazSpec2)
    )
  )

}

