package com.nrinaudo.fetch.facebook

import com.nrinaudo.fetch.QueryString._
import com.nrinaudo.fetch.Request.HttpEngine
import com.nrinaudo.fetch._
import com.nrinaudo.fetch.json4s._
import org.json4s.JsonAST.JValue

class GraphApi(val req: Request[JValue]) {
  def me: JValue = (req / "me").apply()
}

object GraphApi {
  val RootUri = Protocol.Https.host("graph.facebook.com")

  def apply(token: String)(implicit engine: HttpEngine) =
    new GraphApi(RootUri.toRequest.accept(MediaType.Json).map {
      case Status.Success(res) => res.body.as[JValue]
      case res                 => throw new FacebookException(res.body.as[JValue] \ "error")
    } & "access_token" -> token)
}