package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import com.nrinaudo.fetch.Request.Engine
import java.net.URL
import java.io.IOException
import com.nrinaudo.fetch.Encoding.Encodings
import java.nio.charset.Charset
import java.util.Locale
import java.text.DecimalFormat

object Request {
  type Engine = (URL, String, Option[RequestEntity], Headers) => Response[ResponseEntity]

  def apply(url: URL)(implicit engine: Engine): Request = Request(engine, url)

  // TODO: have the version number be dynamic, somehow.
  val UserAgent = "Fetch/0.1"

  private val qFormat = new DecimalFormat("0.###")

  /** Represents a content negotiation header. */
  case class Conneg[T](value: T, q: Float = 1) {
    require(q >= 0 && q <= 1, "q must be between 0 and 1, inclusive.")

    override def toString: String =
      if(q == 1) value.toString
      else       value + ";q=" + qFormat.format(q)

    def map[S](f: T => S): Conneg[S] = Conneg(f(value), q)
    def flatMap[S](f: T => Conneg[S]): Conneg[S] = f(value).copy(q = q)
  }
}

/** Represents an HTTP(S) request.
  *
  * @param engine    engine used to execute the request.
  * @param url       URL to connect to.
  * @param method    HTTP method to execute.
  * @param headers   HTTP headers to send.
  * @param encodings supported response encodings.
  */
case class Request(engine:    Engine,
                   url:       URL,
                   method:    String    = "GET",
                   headers:   Headers   = Map(),
                   encodings: Encodings = Encoding.DefaultEncodings)
  extends (Option[RequestEntity] => Response[ResponseEntity]) {
  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Decodes the specified response according to whatever is specified in the `Content-Encoding` header and the list
    * of encodings we actually support.
    *
    * Unsupported encodings result in IOExceptions.
    */
  private def decode(response: Response[ResponseEntity]): Response[ResponseEntity] =
    response.headers.get("Content-Encoding") map {e =>
      e.reverse.foldRight(response) {(encoding, res) =>
        encodings get encoding map {encoding => res.map(_.decode(encoding))} getOrElse {
          throw new IOException("Unsupported content encoding: " + encoding)
        }
      }
    } getOrElse response


  def apply(body: Option[RequestEntity]): Response[ResponseEntity] = {
    var h = headers

    // Sets body specific HTTP headers (or unsets them if necessary).
    body foreach {b =>
      h = h + ("Content-Type" -> List(b.mimeType.toString))
      if(b.encoding == Encoding.Identity) h = h - "Content-Encoding"
      else                                h = h + ("Content-Encoding" -> List(b.encoding.name))
    }

    // Sets default headers.
    def setDefault(name: String, value: String) =
      if(!headers.contains(name)) h = h + (name -> List(value))

    setDefault("User-Agent", Request.UserAgent)

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
  import Request.Conneg

  private def conneg[T](name: String, values: Conneg[T]*) = {
    header(name, values.map(_.toString) :_*)
  }

  def acceptEncoding(encoding: Conneg[Encoding]*): Request =
    conneg("Accept-Encoding", encoding :_*).copy(encodings =
      encoding.foldLeft(encodings) {(map, e) =>
        map + (e.value.name -> e.value)
      })

  def acceptGzip: Request = acceptEncoding(Encoding.Gzip)

  def acceptDeflate: Request = acceptEncoding(Encoding.Deflate)


  def accept(mimeTypes: Conneg[MimeType]*): Request = conneg("Accept", mimeTypes map {mime =>
    mime.map {_.copy(params = Map()).toString}
  } :_*)

  def acceptCharset(charsets: Conneg[Charset]*): Request = conneg("Accept-Charset", charsets map {c =>
    c.map(_.name())
  } :_*)

  def acceptLanguage(languages: Conneg[Locale]*): Request = conneg("Accept-Language", languages map {l =>
    l.map {language =>
      var value = language.getLanguage
      if(!language.getCountry.isEmpty) value += "-" + language.getCountry
      value
    }
  } :_*)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header(name: String, value: String*): Request = copy(headers = headers + (name -> value.toList))

  def header(name: String): Option[List[String]] = headers.get(name)



  // - Misc. helpers ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def userAgent(name: String): Request = header("User-Agent", name)

  def auth(user: String, pwd: String): Request =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

