// - Project metadata --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
name := "fetch"

organization := "com.nrinaudo"

version := "0.1"

scalaVersion := "2.10.3"



// - Compilation -------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javaOptions := Seq("-Djava.awt.headless=true")

crossPaths := false



// - Dependencies ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
libraryDependencies += "commons-codec"   % "commons-codec"     % "1.5"

libraryDependencies += "org.scalatest"  %% "scalatest"         % "1.9.1"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.10.1" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.10.1" % "test"

libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.7.1"  % "test"

libraryDependencies += "net.databinder" %% "unfiltered-jetty"  % "0.7.1"  % "test"

libraryDependencies += "org.slf4j"      %  "slf4j-nop"         % "1.7.7"  % "test"
