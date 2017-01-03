sonatypeSettings

name := "fetch-tagsoup"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup"      % "1.2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml"   % "1.0.6"

libraryDependencies += "org.scalacheck"         %% "scalacheck"  % "1.13.4" % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"   % "1.7.12"  % "test"
