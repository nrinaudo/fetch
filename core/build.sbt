sonatypeSettings

name := "fetch-core"

libraryDependencies += "commons-codec"          % "commons-codec"             % "1.10"

libraryDependencies += "org.scalatest"          %% "scalatest"                % "3.0.1"  % "test"

libraryDependencies += "org.scalacheck"         %% "scalacheck"               % "1.13.4" % "test"

libraryDependencies += "ws.unfiltered"         %% "unfiltered-filter"        % "0.9.0-beta2"  % "test"

libraryDependencies += "ws.unfiltered"         %% "unfiltered-jetty"         % "0.9.0-beta2"  % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"                % "1.7.22"  % "test"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq("org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5")
    case _                                         => Seq.empty
  }
}
