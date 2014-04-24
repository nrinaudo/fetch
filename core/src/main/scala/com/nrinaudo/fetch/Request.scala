package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import com.nrinaudo.fetch.Request.Engine
import java.net.URL
import java.util.{Locale, Date}
import Headers._
import Conneg._
import java.nio.charset.Charset

object Request {
  type Engine = (URL, String, Option[RequestEntity], Headers) => Response[ResponseEntity]

  def apply(url: URL)(implicit engine: Engine): Request = Request(engine, url)

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
case class Request(engine:    Engine,
                   url:       URL,
                   method:    String  = "GET",
                   headers:   Headers = new Headers()) extends (Option[RequestEntity] => Response[ResponseEntity]) {
  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Decodes the specified response according to whatever is specified in the `Content-Encoding` header and the list
    * of encodings we actually support.
    *
    * Unsupported encodings result in IOExceptions.
    */
  private def decode(response: Response[ResponseEntity]): Response[ResponseEntity] =
    response.headers.get[Seq[Encoding]]("Content-Encoding") map { values =>
      values.reverse.foldLeft(response) { (res, encoding) => res.map(_.decode(encoding))}
    } getOrElse response

  def apply(body: Option[RequestEntity]): Response[ResponseEntity] = {
    var h = headers

    // Sets body specific HTTP headers (or unsets them if necessary).
    body foreach {b =>
      h = h.set("Content-Type", b.mimeType)
      if(b.encoding == Encoding.Identity) h = h.remove("Content-Encoding")
      else                                h = h.set("Content-Encoding", b.encoding)
    }

    h = h.setIfEmpty("User-Agent", Request.UserAgent)

    // Executes the query and decodes the response.
    decode(engine(url, method, body, h))
  }


  def apply(): Response[ResponseEntity] = apply(None)

  def apply(body: RequestEntity): Response[ResponseEntity] = apply(Some(body))



  // - HTTP methods ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def method(method: String): Request = copy(method = method)

  def GET: Request = method("GET")

  def POST: Request = method("POST")

  def PUT: Request = method("PUT")

  def DELETE: Request = method("DELETE")

  def HEAD: Request = method("HEAD")

  def OPTIONS: Request = method("OPTIONS")

  def TRACE: Request = method("TRACE")

  def CONNECT: Request = method("CONNECT")

  def PATCH: Request = method("PATCH")

  def LINK: Request = method("LINK")

  def UNLINK: Request = method("UNLINK")



  // - Content negotiation ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def acceptEncoding(encoding: Conneg[Encoding]*): Request = header("Accept-Encoding", encoding)

  def acceptGzip: Request = acceptEncoding(Encoding.Gzip)

  def acceptDeflate: Request = acceptEncoding(Encoding.Deflate)

  def accept(mimeTypes: Conneg[MimeType]*): Request = header("Accept", mimeTypes map {mime =>
    mime.map {_.copy(params = Map())}
  })

  def acceptCharset(charsets: Conneg[Charset]*): Request = header("Accept-Charset", charsets)

  def acceptLanguage(languages: Conneg[Locale]*): Request = header("Accept-Language", languages)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header[T : HeaderFormat](name: String, value: T): Request = copy(headers = headers.set(name, value))

  def header[T : HeaderFormat](name: String): Option[T] = headers.get[T](name)



  // - Misc. helpers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def range(ranges: ByteRange*): Request =
    if(ranges.isEmpty) this
    else               header("Range", ranges)

  def date(date: Date = new Date()): Request = copy(headers = headers.set("Date", date))

  def userAgent(name: String): Request = header("User-Agent", name)

  // TODO: do we want to wrap user & pwd in an Authorization case class?
  def auth(user: String, pwd: String): Request =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}