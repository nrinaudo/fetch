lazy val root = Project(id = "fetch", base = file("."))
  .aggregate(core, json4sNative, json4sJackson, tagsoup, httpGrammar, nekohtml)
  .settings(noPublishSettings:_*)

lazy val core = project.dependsOn(httpGrammar)

lazy val httpGrammar = Project(id   = "http-grammar",
                                base = file("http-grammar"))

lazy val json4sNative = Project(id   = "json4s-native",
                                base = file("json4s-native")).dependsOn(core)

lazy val json4sJackson = Project(id   = "json4s-jackson",
                                 base = file("json4s-jackson")).dependsOn(core)

lazy val tagsoup = project.dependsOn(core)

lazy val nekohtml = project.dependsOn(core)

lazy val docs = project.dependsOn(core, tagsoup, json4sNative, json4sJackson)
  .settings(unidocSettings:_*)
  .settings(noPublishSettings:_*)

lazy val noPublishSettings = Seq(
  publish         := (),
  publishLocal    := (),
  publishArtifact := false
)
