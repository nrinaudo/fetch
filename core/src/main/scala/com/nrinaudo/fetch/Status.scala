package com.nrinaudo.fetch

object Status {
  val Ok: Status = StatusImpl(200)
  val Created: Status = StatusImpl(201)
  val Accepted: Status = StatusImpl(202)
  val NonAuthoritativeInformation: Status = StatusImpl(203)
  val NoContent: Status = StatusImpl(204)
  val ResetContent: Status = StatusImpl(205)
  val PartialContent: Status = StatusImpl(206)

  val MultipleChoices: Status = StatusImpl(300)
  val MovedPermanently: Status = StatusImpl(301)
  val Found: Status = StatusImpl(302)
  val SeeOther: Status = StatusImpl(303)
  val NotModified: Status = StatusImpl(304)
  val UseProxy: Status = StatusImpl(305)
  val TemporaryRedirect: Status = StatusImpl(307)

  val BadRequest: Status = StatusImpl(400)
  val Unauthorized: Status = StatusImpl(401)
  val PaymentRequired: Status = StatusImpl(402)
  val Forbidden: Status = StatusImpl(403)
  val NotFound: Status = StatusImpl(404)
  val MethodNotAllowed: Status = StatusImpl(405)
  val NotAcceptable: Status = StatusImpl(406)
  val ProxyAuthenticationRequired: Status = StatusImpl(407)
  val RequestTimeout: Status = StatusImpl(408)
  val Conflict: Status = StatusImpl(409)
  val Gone: Status = StatusImpl(410)
  val LengthRequired: Status = StatusImpl(411)
  val PreconditionFailed: Status = StatusImpl(412)
  val RequestEntityTooLarge: Status = StatusImpl(413)
  val RequestUriTooLong: Status = StatusImpl(414)
  val UnsupportedMediaType: Status = StatusImpl(415)
  val RequestRangeNotSatisfiable: Status = StatusImpl(416)
  val ExpectationFailed: Status = StatusImpl(417)

  val InternalServerError: Status = StatusImpl(500)
  val NotImplemented: Status = StatusImpl(501)
  val BadGateway: Status = StatusImpl(502)
  val ServiceUnavailable: Status = StatusImpl(503)
  val GatewayTimeout: Status = StatusImpl(504)
  val HttpVersionNotSupported: Status = StatusImpl(505)

  private case class StatusImpl(code: Int) extends Status

  // TODO: this is not a good apply method, it throws.
  def apply(value: Int): Status =
    if(value > 0 && value < 600) StatusImpl(value)
    else                         throw new IllegalArgumentException("Illegal status: " + value)
}

sealed trait Status {
  val code: Int

  override def toString = code.toString

  def isClientError = code >= 400 && code < 500
  def isServerError = code >= 500 && code < 600
  def isRedirection = code >= 300 && code < 400
  def isSuccess     = code >= 200 && code < 300
  def isError       = isClientError || isServerError
}
