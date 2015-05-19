sonatypeSettings

name := "fetch-core"

libraryDependencies += "commons-codec"          % "commons-codec"             % "1.10"

libraryDependencies += "org.scalatest"          %% "scalatest"                % "2.2.5"  % "test"

libraryDependencies += "org.scalacheck"         %% "scalacheck"               % "1.12.2" % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-filter"        % "0.8.4"  % "test"

libraryDependencies += "net.databinder"         %% "unfiltered-jetty"         % "0.8.4"  % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"                % "1.7.12"  % "test"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
