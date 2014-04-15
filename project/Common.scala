import sbt._
import Keys._

object Common extends Build {
  val pom = {
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

  override val settings = super.settings ++ xerial.sbt.Sonatype.sonatypeSettings ++
  Seq(organization := "com.nrinaudo",
      version      := "0.1",
      scalaVersion := "2.10.3",
      pomExtra     := pom)

}