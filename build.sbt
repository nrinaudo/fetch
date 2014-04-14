
scalaVersion := "2.10.3"

version := "0.1"

sonatypeSettings

organization := "com.nrinaudo"

pomExtra := {
  <url>https://github.com/nrinaudo/fetch</url>
  <licenses>
    <license>
      <name>GNU Library or Lesser General Public License (LGPL)</name>
      <url>http://www.gnu.org/licenses/lgpl.html</url>
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


lazy val root = project.in(file(".")).aggregate(core, json4sNative, json4sJackson)

lazy val core = project

lazy val json4sNative = Project(id   = "json4s-native",
                                base = file("json4s-native")) dependsOn(core)

lazy val json4sJackson = Project(id   = "json4s-jackson",
                                 base = file("json4s-jackson")) dependsOn(core)
