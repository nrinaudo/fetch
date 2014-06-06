package com.nrinaudo.fetch

object Status {
  val Ok = Status(200)
  val Created = Status(201)
  val Accepted = Status(202)
  val NonAuthoritativeInformation = Status(203)
  val NoContent = Status(204)
  val ResetContent = Status(205)
  val PartialContent = Status(206)

  val MultipleChoices = Status(300)
  val MovedPermanently = Status(301)
  val Found = Status(302)
  val SeeOther = Status(303)
  val NotModified = Status(304)
  val UseProxy = Status(305)
  val TemporaryRedirect = Status(307)

  val BadRequest = Status(400)
  val Unauthorized = Status(401)
  val PaymentRequired = Status(402)
  val Forbidden = Status(403)
  val NotFound = Status(404)
  val MethodNotAllowed = Status(405)
  val NotAcceptable = Status(406)
  val ProxyAuthenticationRequired = Status(407)
  val RequestTimeout = Status(408)
  val Conflict = Status(409)
  val Gone = Status(410)
  val LengthRequired = Status(411)
  val PreconditionFailed = Status(412)
  val RequestEntityTooLarge = Status(413)
  val RequestUriTooLong = Status(414)
  val UnsupportedMediaType = Status(415)
  val RequestRangeNotSatisfiable = Status(416)
  val ExpectationFailed = Status(417)

  val InternalServerError = Status(500)
  val NotImplemented = Status(501)
  val BadGateway = Status(502)
  val ServiceUnavailable = Status(503)
  val GatewayTimeout = Status(504)
  val HttpVersionNotSupported = Status(505)
}

case class Status(code: Int) {
  require(code > 0 && code < 600)

  override def toString = code.toString

  def isClientError = code >= 400 && code < 500
  def isServerError = code >= 500 && code < 600
  def isRedirection = code >= 300 && code < 400
  def isSuccess     = code >= 200 && code < 300
  def isError       = isClientError || isServerError
}
