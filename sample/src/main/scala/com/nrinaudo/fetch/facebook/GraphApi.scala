package com.nrinaudo.fetch.facebook

import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Request.HttpEngine
import com.nrinaudo.fetch.QueryString._
import scala.concurrent.{Future, ExecutionContext}
import org.json4s.JsonAST.JValue
import com.nrinaudo.fetch.json4s._

class GraphApi(val req: Request[JValue]) {
  def me: Future[JValue] = (req / "me").apply()
}

object GraphApi {
  val RootUri = Protocol.Https :/ "graph.facebook.com"

  private def filterErrors(res: Response[ResponseEntity]): JValue =
    if(res.status.isSuccess) res.body.as[JValue]
    else                     throw new FacebookException(res.body.as[JValue] \ "error")

  def apply(token: String)(implicit engine: HttpEngine, context: ExecutionContext) =
    new GraphApi(RootUri.map(filterErrors) & "access_token" -> token)
}