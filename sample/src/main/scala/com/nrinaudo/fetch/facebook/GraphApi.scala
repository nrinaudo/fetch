package com.nrinaudo.fetch.facebook

import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Request.HttpEngine
import com.nrinaudo.fetch.QueryString._
import org.json4s.JsonAST.JValue
import com.nrinaudo.fetch.json4s._

class GraphApi(val req: Request[JValue]) {
  def me: JValue = (req / "me").apply()
}

object GraphApi {
  val RootUri = Protocol.Https :/ "graph.facebook.com"

  def apply(token: String)(implicit engine: HttpEngine) =
    new GraphApi(RootUri.accept(MediaType.Json).map {
      case res @ Status.Success(_) => res.body.as[JValue]
      case res                     => throw new FacebookException(res.body.as[JValue] \ "error")
    } & "access_token" -> token)
}