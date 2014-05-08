package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import java.util.{Locale, Date}
import HeaderFormat._
import java.nio.charset.Charset
import scala.concurrent.{ExecutionContext, Future}
import Request._

object Request {
  /** Type for underlying HTTP engines.
    *
    * Fetch comes with a default, `java.net.URLConnection` based [[com.nrinaudo.fetch.net.UrlEngine implementation]],
    * but it might not be applicable to all use-cases. Defining your own engine allows you to use another underlying
    * library, such as [[http://hc.apache.org/httpclient-3.x/ Apache HTTP client ]].
    */
  type Engine = (Url, Method, Option[RequestEntity], Headers) => Future[Response[ResponseEntity]]

  def apply(url: Url)(implicit engine: Engine): Request = Request(engine, url)

  // TODO: have the version number be dynamic, somehow.
  val UserAgent = "Fetch/0.2"
}

/** Represents an HTTP(S) request.
  *
  * @param engine    engine used to execute the request.
  * @param url       URL to connect to.
  * @param method    HTTP method to execute.
  * @param headers   HTTP headers to send.
  */
case class Request(engine:  Engine,
                   url:     Url,
                   method:  Method  = Method.GET,
                   headers: Headers = new Headers(Map[String, String]())) {
  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Decodes the specified response according to whatever is specified in the `Content-Encoding` header and the list
    * of encodings we actually support.
    *
    * Unsupported encodings result in IOExceptions.
    */
  private def decode(response: Response[ResponseEntity]): Response[ResponseEntity] =
    response.contentEncoding.fold(response) { values =>
      values.reverse.foldLeft(response) { (res, encoding) => res.map(_.decode(encoding)) }
    }

  def apply(body: Option[RequestEntity])(implicit context: ExecutionContext): Future[Response[ResponseEntity]] = {
    var h = headers

    // Sets body specific HTTP headers (or unsets them if necessary).
    body foreach {b =>
      h = h.set("Content-Type", b.mimeType)
      if(b.encoding == Encoding.Identity) h = h.remove("Content-Encoding")
      else                                h = h.set("Content-Encoding", b.encoding)
    }

    // I'm not entirely happy with forcing a default Accept header - it's perfectly legal for it to be empty. The
    // standard URLConnection forces a somewhat messed up default, however (image/gif, what were they thinking?),
    // and */* is curl's default behaviour - if it's good enough for curl, it's good enough for me.
    h = h.setIfEmpty("User-Agent", Request.UserAgent).setIfEmpty("Accept", "*/*")

    // Executes the query and decodes the response.
    engine(url, method, body, h) map decode
  }


  def apply()(implicit context: ExecutionContext): Future[Response[ResponseEntity]] = apply(None)

  def apply(body: RequestEntity)(implicit context: ExecutionContext): Future[Response[ResponseEntity]] = apply(Some(body))



  // - HTTP methods ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def method(method: Method): Request = copy(method = method)

  def GET: Request = method(Method.GET)

  def POST: Request = method(Method.POST)

  def PUT: Request = method(Method.PUT)

  def DELETE: Request = method(Method.DELETE)

  def HEAD: Request = method(Method.HEAD)

  def OPTIONS: Request = method(Method.OPTIONS)

  def TRACE: Request = method(Method.TRACE)

  def CONNECT: Request = method(Method.CONNECT)

  def PATCH: Request = method(Method.PATCH)

  def LINK: Request = method(Method.LINK)

  def UNLINK: Request = method(Method.UNLINK)



  // - Content negotiation ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Notifies the remote server about transfer encoding preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3 Accept-Encoding]] header.
    * 
    * @param  encodings list of encodings to declare.
    */
  def acceptEncoding(encodings: Conneg[Encoding]*): Request = header("Accept-Encoding", encodings)

  /** Notifies the remote server that we expect GZIPed responses. */
  def acceptGzip: Request = acceptEncoding(Encoding.Gzip)

  /** Notifies the remote server that we expect deflated responses. */
  def acceptDeflate: Request = acceptEncoding(Encoding.Deflate)

  /** Notifies the remote server about response content type preferences.
    *
    * Note that MIME type parameters are removed here. This is not RFC compliant and is an acknowledged issue.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1 Accept]] header.
    *
    * @param mimeTypes list of MIME types to declare.
    */
  def accept(mimeTypes: Conneg[MimeType]*): Request = header("Accept", mimeTypes map {mime =>
    mime.map {_.copy(params = Map())}
  })

  /** Notifies the remote server about response charset preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2 Accept-Charset]] header.
   *
   * @param charsets list of charsets to declare.
   */
  def acceptCharset(charsets: Conneg[Charset]*): Request = header("Accept-Charset", charsets)

  /** Notifies the remote server about response language preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 Accept-Language]] header.
    *
    * @param languages list of languages to declare.
    */
  def acceptLanguage(languages: Conneg[Locale]*): Request = header("Accept-Language", languages)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the value of the specified header.
    *
    * This method expects an appropriate implicit [[HeaderWriter]] to be in scope. Standard formats are declared
    * in [[Header$ Header]].
    */
  def header[T: HeaderWriter](name: String, value: T): Request = copy(headers = headers.set(name, value))




  // - Cache headers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def ifModifiedSince(date: Date): Request = header("If-Modified-Since", date)

  def ifUnmodifiedSince(date: Date): Request = header("If-Unmodified-Since", date)

  def ifNoneMatch(tags: ETag*): Request = header("If-None-Match", tags)

  def ifMatch(tags: ETag*): Request = header("If-Match", tags)

  def ifRange(tag: ETag): Request = header("If-Range", tag)

  def ifRange(date: Date): Request = header("If-Range", date)



  // - Misc. helpers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def range(ranges: ByteRange*): Request =
    if(ranges.isEmpty) this
    else               header("Range", ranges)

  def date(date: Date = new Date()): Request = header("Date", date)

  def userAgent(name: String): Request = header("User-Agent", name)

  def maxForwards(value: Int): Request = header("Max-Forwards", value)

  // TODO: do we want to wrap user & pwd in an Authorization case class?
  def auth(user: String, pwd: String): Request =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}