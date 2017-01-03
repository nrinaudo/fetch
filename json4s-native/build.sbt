sonatypeSettings

name := "fetch-json4s-native"

libraryDependencies += "org.json4s"     %% "json4s-native"     % "3.5.0"

libraryDependencies += "org.scalatest"  %% "scalatest"         % "3.0.1"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.13.4" % "test"

libraryDependencies += "ws.unfiltered"  %% "unfiltered-filter" % "0.9.0-beta2"  % "test"

libraryDependencies += "ws.unfiltered"  %% "unfiltered-jetty"  % "0.9.0-beta2"  % "test"

libraryDependencies += "org.slf4j"      %  "slf4j-nop"         % "1.7.22" % "test"
