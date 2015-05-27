organization := "org.goldenport"

name := "goldenport-scalaz-lib"

version := "2.0.0"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.2"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.0.0" % "test"
// "org.scalatest" %% "scalatest" % "2.2.4"

//
publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
