sonatypeSettings

name := "fetch-tagsoup"

libraryDependencies += "org.ccil.cowan.tagsoup" % "tagsoup"      % "1.2.1"

libraryDependencies += "org.scalacheck"         %% "scalacheck"  % "1.12.2" % "test"

libraryDependencies += "org.slf4j"              %  "slf4j-nop"   % "1.7.12"  % "test"

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
    case _ =>
      libraryDependencies.value
  }
}