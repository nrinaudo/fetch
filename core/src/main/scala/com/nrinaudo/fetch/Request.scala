package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import java.util.{Locale, Date}
import Headers._
import java.nio.charset.Charset

object Request {
  /** Type for underlying HTTP engines.
    *
    * Fetch comes with a default, `java.net.URLConnection` based [[com.nrinaudo.fetch.net.UrlEngine implementation]],
    * but it might not be applicable to all use-cases. Defining your own engine allows you to use another underlying
    * library, such as [[http://hc.apache.org/httpclient-3.x/ Apache HTTP client]].
    */
  type HttpEngine = (Url, Method, Option[RequestEntity], Headers) => Response[ResponseEntity]

  /** Decodes the specified response according to whatever is specified in the `Content-Encoding` header and the list
    * of encodings we actually support.
    *
    * Unsupported encodings result in IOExceptions.
    */
  private def decode(response: Response[ResponseEntity]): Response[ResponseEntity] =
    response.contentEncoding.fold(response) { values =>
      values.foldRight(response) { (encoding, res) => res.map(_.decode(encoding))}
    }

  private def http(f: HttpEngine): HttpEngine = (url, method, body, headers) => {
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
    h = h.setIfEmpty("User-Agent", UserAgent).setIfEmpty("Accept", "*/*")

    // Executes the query and decodes the response.
    decode(f(url, method, body, h))
  }

  def apply(url: Url)(implicit engine: HttpEngine): Request[Response[ResponseEntity]] =
    new RequestImpl[Response[ResponseEntity]](url, Method.GET, new Headers(), http(engine))

  // TODO: have the version number be dynamic, somehow.
  val UserAgent = "Fetch/0.2"
}

trait Request[A] {
  // - Fields ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val url: Url
  val method: Method
  val headers: Headers



  // - Abstract methods ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def copy(url: Url, method: Method, headers: Headers): Request[A]
  def apply(body: Option[RequestEntity]): A
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

  def acceptEncoding: Option[Seq[Conneg[Encoding]]] = header[Seq[Conneg[Encoding]]]("Accept-Encoding")

  /** Notifies the remote server that we expect GZIPed responses. */
  def acceptGzip: Request[A] = acceptEncoding(Encoding.Gzip)

  /** Notifies the remote server that we expect deflated responses. */
  def acceptDeflate: Request[A] = acceptEncoding(Encoding.Deflate)

  /** Notifies the remote server about response content type preferences.
    *
    * Note that MIME type parameters are removed here. This is not RFC compliant and is an acknowledged issue.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.1 Accept]] header.
    *
    * @param mimeTypes list of MIME types to declare.
    */
  def accept(mimeTypes: Conneg[MimeType]*): Request[A] = header("Accept", mimeTypes.map(_.map(_.clearParams)))

  def accept: Option[Seq[Conneg[MimeType]]] = header[Seq[Conneg[MimeType]]]("Accept")

  /** Notifies the remote server about response charset preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.2 Accept-Charset]] header.
    *
    * @param charsets list of charsets to declare.
    */
  def acceptCharset(charsets: Conneg[Charset]*): Request[A] = header("Accept-Charset", charsets)

  def acceptCharset: Option[Seq[Conneg[Charset]]] = header[Seq[Conneg[Charset]]]("Accept-Charset")

  /** Notifies the remote server about response language preferences.
    *
    * This maps to the [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.4 Accept-Language]] header.
    *
    * @param languages list of languages to declare.
    */
  def acceptLanguage(languages: Conneg[Locale]*): Request[A] = header("Accept-Language", languages)

  def acceptLanguage: Option[Seq[Conneg[Locale]]] = header[Seq[Conneg[Locale]]]("Accept-Language")



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the value of the specified header.
    *
    * This method expects an appropriate implicit [[ValueWriter]] to be in scope. Standard formats are declared
    * in [[Headers$ Headers]].
    */
  def header[T: ValueWriter](name: String, value: T): Request[A] = headers(headers.set(name, value))

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
                             private val engine:   (Url, Method, Option[RequestEntity], Headers) => A)
  extends Request[A] {
  override def copy(url: Url, method: Method, headers: Headers): Request[A] =
    new RequestImpl[A](url, method, headers, engine)

  override def apply(body: Option[RequestEntity]): A = engine.apply(url, method, body, headers)

  override def map[B](f: (A) => B): Request[B] =
    new RequestImpl(url, method, headers, (a, b, c, d) => f(engine(a, b, c, d)))
}