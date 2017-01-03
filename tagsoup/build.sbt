sonatypeSettings

name := "fetch-tagsoup"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup"      % "1.2.1"

libraryDependencies += "org.scalacheck"         %% "scalacheck"  % "1.13.4" % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"   % "1.7.22"  % "test"


libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 => Seq("org.scala-lang.modules" %% "scala-xml" % "1.0.6")
    case _                                         => Seq.empty
  }
}
