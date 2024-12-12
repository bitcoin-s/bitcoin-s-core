import sbt.Keys.excludeLintKeys

import scala.util.Properties

val scala2_13 = "2.13.16"
val scala3 = "3.6.2"

ThisBuild / scalafmtOnCompile := !Properties.envOrNone("CI").contains("true")

ThisBuild / scalaVersion := scala3

ThisBuild / crossScalaVersions := List(/*scala2_13,*/ scala3)

//https://github.com/sbt/sbt/pull/5153
//https://github.com/bitcoin-s/bitcoin-s/pull/2194
Global / excludeLintKeys ++= Set(
  com.typesafe.sbt.packager.Keys.maintainer,
  Keys.mainClass,
  com.github.sbt.git.SbtGit.GitKeys.gitRemoteRepo
)

//needed so that we can use our versions with docker
//see: https://github.com/dwijnand/sbt-dynver#portable-version-strings
//https://github.com/bitcoin-s/bitcoin-s/issues/2672
ThisBuild / dynverSeparator := "-"

//don't require the leading 'v' on dynver versioning
//as that doesn't work with windows or mac versioning
ThisBuild / dynverVTagPrefix := false
