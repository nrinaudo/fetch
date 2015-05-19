package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import java.util.Date
import Headers._
import java.nio.charset.Charset
import java.net.URI

object Request {
  /** Type for underlying HTTP engines.
    *
    * Fetch comes with a default, `java.net.URLConnection` based [[com.nrinaudo.fetch.net.UrlEngine implementation]],
    * but it might not be applicable to all use-cases. Defining your own engine allows you to use another underlying
    * library, such as [[http://hc.apache.org/httpclient-3.x/ Apache HTTP client]].
    */
  type HttpEngine = (Url, Method, Option[RequestEntity], Headers) => Response[ResponseEntity]

  private def http(f: HttpEngine): HttpEngine = (url, method, body, headers) => {
    var h = headers

    // Sets body specific HTTP headers (or unsets them if necessary).
    body foreach {b =>
      h = h.set("Content-Type", b.mediaType)
      if(b.encoding == Encoding.Identity) h = h.remove("Content-Encoding")
      else                                h = h.set("Content-Encoding", b.encoding)
    }

    // I'm not entirely happy with forcing a default Accept header - it's perfectly legal for it to be empty. The
    // standard URLConnection forces a somewhat messed up default, however (image/gif, what were they thinking?),
    // and */* is curl's default behaviour - if it's good enough for curl, it's good enough for me.
    h = h.setIfEmpty("User-Agent", UserAgent).setIfEmpty("Accept", "*/*")

    f(url, method, body, h)
  }

  def from(uri: URI)(implicit engine: HttpEngine): Option[Request[Response[ResponseEntity]]] =
    Url.fromUri(uri).map(apply)

  def from(url: String)(implicit engine: HttpEngine): Option[Request[Response[ResponseEntity]]] =
    Url.parse(url).map(apply)

  /**
   * Creates a new instance of [[Request]].
   *
   * The newly created instance will default to [[Method.GET]].
   *
   * @param url    url on which the request will be performed.
   * @param engine HTTP engine to use when performing the request.
   */
  def apply(url: Url)(implicit engine: HttpEngine): Request[Response[ResponseEntity]] =
    new RequestImpl[Response[ResponseEntity]](url, Method.GET, new Headers(), http(engine))

  // TODO: have the version number be dynamic, somehow.
  val UserAgent = "Fetch/0.2"
}

/** Represents an HTTP request.
  *
  * Instances are created through the companion object. Once an instance is obtained, the request can be
  * configured through "raw" modification methods ({{{method}}}, {{{headers}}}...) as well as specialised helpers such
  * as {{{GET}}}, {{{acceptGzip}}} or {{{/}}}.
  */
trait Request[A] {
  // - Fields ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** URL on which the request will be performed. */
  val url: Url
  /** HTTP method of the request. */
  val method: Method
  /** List of HTTP headers of the request. */
  val headers: Headers



  // - Abstract methods ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  protected def copy(url: Url, method: Method, headers: Headers): Request[A]
  def apply(body: Option[RequestEntity]): A

  /** Applies the specified transformation to the request's eventual response.
    *
    * Application developers should be wary of a common pitfall: when working with responses that contain instances
    * of [[ResponseEntity]], they should always clean these up, either by reading their content (transforming it
    * to something else or calling [[ResponseEntity.empty]]) or explicitly ignoring them (by calling
    * [[ResponseEntity.ignore]]).
    *
    * This is a common source of issues when mapping error statuses to exceptions: each connection will be kept
    * alive until the remote host decides it has timed out.
    */
  def map[B](f: A => B): Request[B]



  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def apply(): A = apply(None)
  def apply(body: RequestEntity): A = apply(Some(body))



  // - Field modification ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def url(value: Url): Request[A] = copy(value, method, headers)
  def method(value: Method): Request[A] = copy(url, value, headers)
  def headers(value: Headers): Request[A] = copy(url, method, value)



  // - Url manipulation ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def /(segment: String): Request[A] = url(url / segment)
  def ?(value: QueryString): Request[A] = url(url ? value)
  def &[T: ValueWriter](param: (String, T)): Request[A] = url(url & param)



  // - HTTP methods ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def GET: Request[A] = method(Method.GET)
  def POST: Request[A] = method(Method.POST)
  def PUT: Request[A] = method(Method.PUT)
  def DELETE: Request[A] = method(Method.DELETE)
  def HEAD: Request[A] = method(Method.HEAD)
  def OPTIONS: Request[A] = method(Method.OPTIONS)
  def TRACE: Request[A] = method(Method.TRACE)
  def CONNECT: Request[A] = method(Method.CONNECT)
  def PATCH: Request[A] = method(Method.PATCH)
  def LINK: Request[A] = method(Method.LINK)
  def UNLINK: Request[A] = method(Method.UNLINK)



  // - Content negotiation ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Notifies the remote server about transfer encoding preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3 Accept-Encoding]] header.
    *
    * @param  encodings list of encodings to declare.
    */
  def acceptEncoding(encodings: Conneg[Encoding]*): Request[A] = header("Accept-Encoding", encodings)

  /** Returns the value of this instance's encoding header. */
  def acceptEncoding: Option[Seq[Conneg[Encoding]]] = header[Seq[Conneg[Encoding]]]("Accept-Encoding")

  /** Notifies the remote server that we accept GZIPed responses. */
  def acceptGzip: Request[A] = acceptEncoding(Encoding.Gzip)

  /** Notifies the remote server that we accept deflated responses. */
  def acceptDeflate: Request[A] = acceptEncoding(Encoding.Deflate)

  /** Notifies the remote server about response content type preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1 Accept]] header.
    *
    * @param types list of media types to declare.
    */
  def accept(types: Conneg[MediaType]*): Request[A] = header("Accept", types)

  /** Returns the value of this instance's content type preferences. */
  def accept: Option[Seq[Conneg[MediaType]]] = header[Seq[Conneg[MediaType]]]("Accept")

  /** Notifies the remote server about response charset preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2 Accept-Charset]] header.
    *
    * @param charsets list of charsets to declare.
    */
  def acceptCharset(charsets: Conneg[Charset]*): Request[A] = header("Accept-Charset", charsets)

  /** Returns the value of this instance's charset preferences. */
  def acceptCharset: Option[Seq[Conneg[Charset]]] = header[Seq[Conneg[Charset]]]("Accept-Charset")

  /** Notifies the remote server about response language preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 Accept-Language]] header.
    *
    * @param languages list of languages to declare.
    */
  def acceptLanguage(languages: Conneg[Language]*): Request[A] = header("Accept-Language", languages)

  /** Returns the value of this instance's language preferences. */
  def acceptLanguage: Option[Seq[Conneg[Language]]] = header[Seq[Conneg[Language]]]("Accept-Language")



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the value of the specified header.
    *
    * This method expects an appropriate implicit [[ValueWriter]] to be in scope. Standard formats are declared
    * in [[Headers$ Headers]].
    */
  def header[T: ValueWriter](name: String, value: T): Request[A] = headers(headers.set(name, value))

  /** Returns the value of the specified header. */
  def header[T: ValueReader](name: String): Option[T] = headers.getOpt[T](name)




  // - Cache headers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def ifModifiedSince(date: Date): Request[A] = header("If-Modified-Since", date)

  def ifModifiedSince: Option[Date] = header[Date]("If-Modified-Since")

  def ifUnmodifiedSince(date: Date): Request[A] = header("If-Unmodified-Since", date)

  def ifUnmodifiedSince: Option[Date] = header[Date]("If-Unmodified-Since")

  def ifNoneMatch(tags: ETag*): Request[A] = header("If-None-Match", tags)

  def ifNoneMatch: Option[Seq[ETag]] = header[Seq[ETag]]("If-None-Match")

  def ifMatch(tags: ETag*): Request[A] = header("If-Match", tags)

  def ifMatch: Option[Seq[ETag]] = header[Seq[ETag]]("If-Match")

  def ifRange(tag: ETag): Request[A] = header("If-Range", tag)

  def ifRange(date: Date): Request[A] = header("If-Range", date)



  // - Misc. helpers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def range(ranges: ByteRange*): Request[A] =
    if(ranges.isEmpty) this
    else               header("Range", ranges)

  def range: Option[Seq[ByteRange]] = header[Seq[ByteRange]]("Range")

  def date(date: Date = new Date()): Request[A] = header("Date", date)

  def date: Option[Date] = header[Date]("Date")

  def userAgent(name: String): Request[A] = header("User-Agent", name)

  def userAgent: Option[String] = header[String]("User-Agent")

  def maxForwards(value: Int): Request[A] = header("Max-Forwards", value)

  def maxForwards: Option[Int] = header[Int]("Max-Forwards")

  // TODO: do we want to wrap user & pwd in an Authorization case class?
  def auth(user: String, pwd: String): Request[A] =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

private class RequestImpl[A](override val url:     Url,
                             override val method:  Method  = Method.GET,
                             override val headers: Headers = new Headers(Map[String, String]()),
                             private val  engine:  (Url, Method, Option[RequestEntity], Headers) => A)
  extends Request[A] {
  override def copy(url: Url, method: Method, headers: Headers): Request[A] =
    new RequestImpl[A](url, method, headers, engine)

  override def apply(body: Option[RequestEntity]): A = engine.apply(url, method, body, headers)

  override def map[B](f: A => B): Request[B] =
    new RequestImpl(url, method, headers, (a, b, c, d) => f(engine(a, b, c, d)))
}