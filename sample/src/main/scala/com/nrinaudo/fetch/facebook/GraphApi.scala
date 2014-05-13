package com.nrinaudo.fetch.facebook

import com.nrinaudo.fetch._
import com.nrinaudo.fetch.Request.HttpEngine
import com.nrinaudo.fetch.QueryString._
import scala.concurrent.{Future, ExecutionContext}
import org.json4s.JsonAST.JValue
import com.nrinaudo.fetch.json4s._
import org.json4s.DefaultFormats

class GraphApi(val req: Request[JValue]) {
  def me: Future[JValue] = (req / "me").apply()
}

object FacebookException {
  implicit val JsonFormat = DefaultFormats
  private def message(value: JValue) = (value \ "message").extractOrElse("Unknown error")
  private def code(value: JValue) = (value \ "code").extractOrElse(-1)
}

class FacebookException(message: String, val code: Int) extends Exception(message) {
  def this(value: JValue) = this(FacebookException.message(value), FacebookException.code(value))
}

object GraphApi {
  val RootUri = Protocol.Https.host("graph.facebook.com")

  def apply(token: String)(implicit engine: HttpEngine, context: ExecutionContext) =
    new GraphApi(Request(RootUri).map(response =>
      if(response.status.isSuccess) response.body.as[JValue]
      else                          throw new FacebookException(response.body.as[JValue] \ "error")
    ) & "access_token" -> token)
}