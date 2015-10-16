sonatypeSettings

name := "fetch-nekohtml"

libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.22"

libraryDependencies := {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      libraryDependencies.value :+ "org.scala-lang.modules" %% "scala-xml" % "1.0.4"
    case _ =>
      libraryDependencies.value
  }
}
