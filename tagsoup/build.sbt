sonatypeSettings

name := "fetch-tagsoup"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup"      % "1.2.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-xml"   % "1.0.3"

libraryDependencies += "org.scalacheck"         %% "scalacheck"  % "1.11.5" % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"   % "1.7.7"  % "test"
