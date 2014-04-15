import SonatypeKeys._

sonatypeSettings

lazy val root = project.in(file(".")).aggregate(core, json4sNative, json4sJackson).settings(packagedArtifacts := Map.empty)

lazy val core = project

lazy val json4sNative = Project(id   = "json4s-native",
                                base = file("json4s-native")) dependsOn(core)

lazy val json4sJackson = Project(id   = "json4s-jackson",
                                 base = file("json4s-jackson")) dependsOn(core)
