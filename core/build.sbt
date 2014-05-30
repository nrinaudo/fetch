sonatypeSettings

instrumentSettings

name := "fetch"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"

libraryDependencies += "commons-codec"           % "commons-codec"            % "1.5"

libraryDependencies += "org.scalatest"          %% "scalatest"                % "2.1.3"  % "test"

libraryDependencies += "org.scalacheck"         %% "scalacheck"               % "1.11.3" % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-filter"        % "0.7.1"  % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-jetty"         % "0.7.1"  % "test"

libraryDependencies += "org.slf4j"               % "slf4j-nop"                % "1.7.7"  % "test"
