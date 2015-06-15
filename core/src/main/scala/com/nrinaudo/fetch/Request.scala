package com.nrinaudo.fetch

import java.io._
import java.net.URI
import java.nio.charset.Charset
import java.util.Date
import Headers._

import com.nrinaudo.fetch.Request.Entity
import org.apache.commons.codec.binary.Base64

object Request {
  trait Entity {
    def contentLength: Option[Long]
    def write(out: OutputStream): Unit
    def mediaType: MediaType
  }

  /** Type for underlying HTTP engines.
    *
    * Fetch comes with a default, `java.net.URLConnection` based [[com.nrinaudo.fetch.net.UrlEngine implementation]],
    * but it might not be applicable to all use-cases. Defining your own engine allows you to use another underlying
    * library, such as [[http://hc.apache.org/httpclient-3.x/ Apache HTTP client]].
    */
  type HttpEngine = (Url, Method, Option[Entity], Parameters) => Response[Response.Entity]

  private def http(f: HttpEngine): HttpEngine = (url, method, body, headers) => f(url, method, body,
    // Sets the content type if applicable.
    body.fold(headers)(b => headers.set("Content-Type", b.mediaType))
      .setIfEmpty("User-Agent", UserAgent))




  def from(uri: URI)(implicit engine: HttpEngine): Option[Request[Response[Response.Entity]]] =
    Url.fromUri(uri).map(apply)

  def from(url: String)(implicit engine: HttpEngine): Option[Request[Response[Response.Entity]]] =
    Url.parse(url).map(apply)

  /**
   * Creates a new instance of [[Request]].
   *
   * The newly created instance will default to [[Method.GET]].
   *
   * @param url    url on which the request will be performed.
   * @param engine HTTP engine to use when performing the request.
   */
  def apply(url: Url)(implicit engine: HttpEngine): Request[Response[Response.Entity]] =
    new Request[Response[Response.Entity]](url, Method.GET, Parameters.empty, http(engine))

  // TODO: have the version number be dynamic, somehow.
  val UserAgent = "Fetch/0.2"
}

/** Represents an HTTP request.
  *
  * Instances are created through the companion object. Once an instance is obtained, the request can be
  * configured through "raw" modification methods ({{{method}}}, {{{headers}}}...) as well as specialised helpers such
  * as {{{GET}}}, {{{acceptGzip}}} or {{{/}}}.
  */
case class Request[A](url: Url, method: Method, headers: Parameters, run: (Url, Method, Option[Entity], Parameters) => A) {
  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private def entity[B](body: B)(implicit writer: EntityWriter[B]): Entity = {
    new Entity {
      override def contentLength =
        if(encoding != Encoding.Identity) None
        else                              writer.length(body)

      override def mediaType = writer.mediaType

      override def write(out: OutputStream) = {
        val eout = encoding.encode(out)
        try { writer.write(body, eout) }
        finally { eout.close() }
      }
    }
  }

  def apply(): A = apply(None)
  def apply[B: EntityWriter](body: B): A = apply(Some(entity(body)))
  def apply(body: Option[Entity]): A = run(url, method, body, headers)

  /** Applies the specified transformation to the request's eventual response.
    *
    * Application developers should be wary of a common pitfall: when working with responses that contain instances
    * of [[Response.Entity]], they should always clean these up, either by reading their content (transforming it
    * to something else or calling [[Response.Entity.empty]]) or explicitly ignoring them (by calling
    * [[Response.Entity.ignore]]).
    *
    * This is a common source of issues when mapping error statuses to exceptions: each connection will be kept
    * alive until the remote host decides it has timed out.
    */
  def map[B](f: A => B): Request[B] =
    copy(run = (a, b, c, d) => f(run(a, b, c, d)))


  // - Field modification ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def url(value: Url): Request[A] = copy(url = value)
  def method(value: Method): Request[A] = copy(method = value)
  def headers(value: Parameters): Request[A] = copy(headers = value)



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

  def acceptEncoding(encoding: Encoding): Request[A] = acceptEncoding(Conneg(encoding, 1f))

  /** Notifies the remote server that we accept GZIPed responses. */
  def acceptGzip: Request[A] = acceptEncoding(Conneg(Encoding.Gzip, 1f))

  /** Notifies the remote server that we accept deflated responses. */
  def acceptDeflate: Request[A] = acceptEncoding(Conneg(Encoding.Deflate, 1f))

  /** Notifies the remote server about response content type preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1 Accept]] header.
    *
    * @param types list of media types to declare.
    */
  def accept(types: Conneg[MediaType]*): Request[A] = header("Accept", types)

  def accept(mediaType: MediaType): Request[A] = accept(Conneg(mediaType, 1f))

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



  // - Entity body headers ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Encodes all request entities with the specified encoding.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.11 Content-Encoding]] header.
    *
    * Since {{{Encoding.Identity}}} is the default value, passing that as argument will cause the corresponding HTTP
    * header to be unset.
    */
  def encoding(encoding: Encoding): Request[A] = {
    if(encoding == Encoding.Identity) {
      headers.remove("Content-Encoding")
      this
    }
    else header("Content-Encoding", encoding)
  }

  /** Returns the content-encoding used by this request. */
  def encoding: Encoding = header[Encoding]("Content-Encoding").getOrElse(Encoding.Identity)

  /** Encodes all request entities using {{{Encoding.Gzip}}}. */
  def gzip: Request[A] = encoding(Encoding.Gzip)

  /** Encodes all request entities using {{{Encoding.Deflate}}}. */
  def deflate: Request[A] = encoding(Encoding.Deflate)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the value of the specified header.
    *
    * This method expects an appropriate implicit [[ValueWriter]] to be in scope. Standard formats are declared
    * in [[Headers$ Headers]].
    */
  def header[T: ValueWriter](name: String, value: T): Request[A] = headers(headers.set(name, value))

  /** Returns the value of the specified header. */
  def header[T: ValueReader](name: String): Option[T] = headers.get[T](name)




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