package fetch

/** Defines known HTTP status codes, as well as pattern match helpers. */
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

  /** Helper for status group extractors.
    *
    * For example, the following code will create an extractor for the `2xx` group ([[Status.Success]], essentially):
    * {{{
    * object Success extends Extractor(_.isSuccess)
    * }}}
    *
    * This can then be used as follows: {{{
    *   val res: Response[ResponseEntity] = ???
    *
    *   // Pattern match against a status
    *   res.status match {
    *     case Success(s) => println("Success")
    *     case _          => println("Not success")
    *   }
    *
    *   // Pattern match against a Response
    *   res match {
    *     case Success(res) => println("Success")
    *     case _            => println("Not success")
    *   }
    * }}}
    *
    * @param f any status for which this returns `true` will be matched by the extractor.
    */
  class Extractor(val f: Status => Boolean) {
    /** Pattern matches against [[Status statuses]]. */
    def unapply(status: Status): Option[Status] =
      if(f(status)) Some(status)
      else          None

    /** Pattern matches against [[Response responses]]. */
    def unapply[T](res: Response[T]): Option[Response[T]] =
      if(f(res.status)) Some(res)
      else              None
  }


  /** Used to pattern match [[Status statuses]] and [[Response responses]] on the `2xx` group. */
  object Success extends Extractor(_.isSuccess)
  /** Used to pattern match [[Status statuses]] and [[Response responses]] on the `4xx` and `5xx` groups. */
  object Error extends Extractor(_.isError)
  /** Used to pattern match [[Status statuses]] and [[Response responses]] on the `4xx` group. */
  object ClientError extends Extractor(_.isClientError)
  /** Used to pattern match [[Status statuses]] and [[Response responses]] on the `5xx` group. */
  object ServerError extends Extractor(_.isServerError)
  /** Used to pattern match [[Status statuses]] and [[Response responses]] on the `3xx` group. */
  object Redirection extends Extractor(_.isRedirection)
}

/** Represents an HTTP status code, as defined in
  * [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec6.html#sec6.1.1 RFC 2616]].
  *
  * This class and its companion objects provide various extraction helpers to ease pattern matching.
  * For example: {{{
  *   val res: Response[ResponseEntity] = ???
  *
  *   res.status match {
  *     case Status.Success(_) => println("Success")
  *     case _                 => println("Not success")
  *   }
  * }}}
  *
  * It's also possible to pattern match directly on instances of [[Response]], which can be useful when
  * [[Request.map mapping]] on [[Request requests]]: {{{
  *   val res: Request[Response[ResponseEntity]] = ???
  *
  *   res map {
  *     // Match on the 2xx group
  *     case res @ Status.Success(_)       => println("Success: " + res.body.as[String])
  *     // Match on HTTP 500
  *     case Status.InternalServerError(_) => println("Internal server error")
  *     // Match on everything else
  *     case _                             => println("Something else entirely")
  *   }
  * }}}
  *
  * See the [[Status$ companion object]] for known status codes.
  */
case class Status(code: Int) {
  require(code > 0 && code < 600)

  /** Allows instances of [[Status]] to be used as extractors for [[Response]].
    *
    * For example: {{{
    *  val req: Request[Response[Response.Entity]] = ???
    *
    *  req.map {
    *    case Status.Ok(res) => res.body.as[String]
    *    case res            => throw new Exception("Unexpected status: " + res.status)
    *  }
    * }}}
    */
  def unapply[T](res: Response[T]): Option[Response[T]] =
    if(res.status == this) Some(res)
    else                   None

  override def toString = code.toString

  /** Checks whether the status is a client error (`4xx`). */
  def isClientError = code >= 400 && code < 500
  /** Checks whether the status is a server error (`5xx`). */
  def isServerError = code >= 500 && code < 600
  /** Checks whether the status is a redirection (`3xx`). */
  def isRedirection = code >= 300 && code < 400
  /** Checks whether the status is a client error (`2xx`). */
  def isSuccess     = code >= 200 && code < 300
  /** Checks whether the status is an error (`4xx` and `5xx`). */
  def isError       = isClientError || isServerError
}
