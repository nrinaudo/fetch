sonatypeSettings

instrumentSettings

name := "fetch-json4s-native"

libraryDependencies += "org.json4s"     %% "json4s-native"     % "3.2.10"

libraryDependencies += "org.scalatest"  %% "scalatest"         % "2.2.2"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.11.5" % "test"

libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.8.1"  % "test"

libraryDependencies += "net.databinder" %% "unfiltered-jetty"  % "0.8.1"  % "test"

libraryDependencies += "org.slf4j"      %  "slf4j-nop"         % "1.7.7"  % "test"
