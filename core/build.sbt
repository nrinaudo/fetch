sonatypeSettings

instrumentSettings

name := "fetch"

libraryDependencies += "commons-codec"           % "commons-codec"            % "1.9"

libraryDependencies += "org.scalatest"          %% "scalatest"                % "2.2.2"  % "test"

libraryDependencies += "org.scalacheck"         %% "scalacheck"               % "1.11.5" % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-filter"        % "0.8.1"  % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-jetty"         % "0.8.1"  % "test"

libraryDependencies += "org.slf4j"               % "slf4j-nop"                % "1.7.7"  % "test"


libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value ++ Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2")
     case _ =>
      libraryDependencies.value
  }
}
