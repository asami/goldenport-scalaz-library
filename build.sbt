organization := "org.goldenport"

name := "goldenport-scalaz-lib"

version := "2.1.0"

scalaVersion := "2.12.7"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"


libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.26"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.1.0" % "test"
// "org.scalatest" %% "scalatest" % "2.2.4"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
