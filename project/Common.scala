import sbt._
import Keys._

object Common extends Build {
  val pom = {
    <url>https://github.com/nrinaudo/fetch</url>
      <licenses>
        <license>
          <name>MIT License</name>
          <url>http://www.opensource.org/licenses/mit-license.php</url>
        </license>
      </licenses>
      <scm>
        <connection>scm:git:github.com/nrinaudo/fetch.git</connection>
        <developerConnection>scm:git:git@github.com:nrinaudo/fetch.git</developerConnection>
        <url>github.com/nrinaudo/fetch.git</url>
      </scm>
      <developers>
        <developer>
          <id>nrinaudo</id>
          <name>Nicolas Rinaudo</name>
          <url>http://nrinaudo.github.io</url>
        </developer>
      </developers>
  }

  override val settings = super.settings ++
                          xerial.sbt.Sonatype.sonatypeSettings ++
                          Seq(organization      :=  "com.nrinaudo",
                             version            :=  "0.3.0-SNAPSHOT",
                             scalaVersion       :=  "2.11.7",
                            javacOptions       ++= Seq("-source", "1.7", "-target", "1.7"),
                             crossScalaVersions := Seq("2.10.6", "2.11.7"),
                             scalacOptions      ++= Seq("-deprecation",
                              "-encoding", "UTF-8",
                              "-feature",
                              "-language:existentials",
                              "-language:higherKinds",
                              "-language:implicitConversions",
                              "-target:jvm-1.7",
                              "-unchecked",
                              "-Xfatal-warnings",
                              "-Xlint",
                              "-Yno-adapted-args",
                              "-Ywarn-dead-code",
                              "-Ywarn-numeric-widen",
                              "-Ywarn-value-discard",
                              "-Xfuture"),
                             incOptions          := incOptions.value.withNameHashing(true),
                             libraryDependencies ++= Seq(
                               "com.github.mpilquist" %% "simulacrum" % "0.4.0",
                               compilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
                             ),
                             pomExtra            := pom)

}
