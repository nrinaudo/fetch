import SonatypeKeys._

lazy val root = Project(id = "fetch",
                        base = file(".")).aggregate(core, json4sNative, json4sJackson, sample, tagsoup).settings(packagedArtifacts := Map.empty)

lazy val core = project

lazy val json4sNative = Project(id   = "json4s-native",
                                base = file("json4s-native")) dependsOn(core)

lazy val json4sJackson = Project(id   = "json4s-jackson",
                                 base = file("json4s-jackson")) dependsOn(core)

lazy val tagsoup = Project(id   = "tagsoup",
                                 base = file("tagsoup")) dependsOn(core)

lazy val sample = Project(id   = "sample",
                          base = file("sample")) dependsOn(core, json4sJackson) settings(packagedArtifacts := Map.empty)
