sonatypeSettings

name := "fetch"

libraryDependencies += "commons-codec"  % "commons-codec"      % "1.10"

libraryDependencies += "org.scalatest"  %% "scalatest"         % "2.2.4"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.12.2" % "test"

libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.8.4"  % "test"

libraryDependencies += "net.databinder" %% "unfiltered-jetty"  % "0.8.4"  % "test"

libraryDependencies += "org.slf4j"      % "slf4j-nop"          % "1.7.10"  % "test"


libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3")
     case _ =>
      libraryDependencies.value
  }
}
