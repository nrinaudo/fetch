sonatypeSettings

name := "fetch-json4s-jackson"

libraryDependencies += "org.json4s"     %% "json4s-jackson"    % "3.2.11"

libraryDependencies += "org.scalatest"  %% "scalatest"         % "2.2.5"  % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck"        % "1.12.2" % "test"

libraryDependencies += "net.databinder" %% "unfiltered-filter" % "0.8.4"  % "test"

libraryDependencies += "net.databinder" %% "unfiltered-jetty"  % "0.8.4"  % "test"

libraryDependencies += "org.slf4j"      %  "slf4j-nop"         % "1.7.12" % "test"
