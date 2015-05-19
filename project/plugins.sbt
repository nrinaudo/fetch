resolvers += Classpaths.sbtPluginReleases

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.2.1")

addSbtPlugin("com.typesafe.sbt" % "sbt-pgp" % "0.8.1")

addSbtPlugin("org.scoverage" %% "sbt-scoverage" % "1.1.0")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.0.0")
