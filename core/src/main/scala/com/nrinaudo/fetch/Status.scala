package com.nrinaudo.fetch

object Status {
  object Ok extends Status(200)
  object Created extends Status(201)
  object Accepted extends Status(202)
  object NonAuthoritativeInformation extends Status(203)
  object NoContent extends Status(204)
  object ResetContent extends Status(205)
  object PartialContent extends Status(206)

  object MultipleChoices extends Status(300)
  object MovedPermanently extends Status(301)
  object Found extends Status(302)
  object SeeOther extends Status(303)
  object NotModified extends Status(304)
  object UseProxy extends Status(305)
  object TemporaryRedirect extends Status(307)

  object BadRequest extends Status(400)
  object Unauthorized extends Status(401)
  object PaymentRequired extends Status(402)
  object Forbidden extends Status(403)
  object NotFound extends Status(404)
  object MethodNotAllowed extends Status(405)
  object NotAcceptable extends Status(406)
  object ProxyAuthenticationRequired extends Status(407)
  object RequestTimeout extends Status(408)
  object Conflict extends Status(409)
  object Gone extends Status(410)
  object LengthRequired extends Status(411)
  object PreconditionFailed extends Status(412)
  object RequestEntityTooLarge extends Status(413)
  object RequestUriTooLong extends Status(414)
  object UnsupportedMediaType extends Status(415)
  object RequestRangeNotSatisfiable extends Status(416)
  object ExpectationFailed extends Status(417)

  object InternalServerError extends Status(500)
  object NotImplemented extends Status(501)
  object BadGateway extends Status(502)
  object ServiceUnavailable extends Status(503)
  object GatewayTimeout extends Status(504)
  object HttpVersionNotSupported extends Status(505)
}

/**
 * Represents HTTP statuses.
 * @author Nicolas Rinaudo
 */
case class Status(code: Int) {
  def isClientError = code >= 400 && code < 500
  def isServerError = code >= 500 && code < 600
  def isRedirection = code >= 300 && code < 400
  def isSuccess     = code >= 200 && code < 300
  def isError       = isClientError || isServerError
}
