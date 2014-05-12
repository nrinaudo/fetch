package com.nrinaudo.fetch.facebook

import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Request.Engine
import com.nrinaudo.fetch.QueryString._

class GraphApi(val req: Request) {
  def me = req / "me"
}

object GraphApi {
  val RootUri = Protocol.Https.host("graph.facebook.com")

  def apply(token: String)(implicit engine: Engine) = new GraphApi(RootUri & "access_token" -> token)
}