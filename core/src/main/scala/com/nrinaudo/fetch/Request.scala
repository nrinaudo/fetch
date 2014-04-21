package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import com.nrinaudo.fetch.Request.Engine
import java.net.URL
import java.io.IOException
import com.nrinaudo.fetch.Encoding.Encodings

object Request {
  type Engine = (URL, String, Option[RequestEntity], Headers) => Response[ResponseEntity]

  def apply(url: URL)(implicit engine: Engine): Request = Request(engine, url)
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


  // - Content encoding ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def acceptEncoding(encoding: Encoding): Request =
    copy(encodings = encodings + (encoding.name -> encoding),
      headers = headers + ("Accept-Encoding" -> List(encoding.name)))

  def acceptGzip: Request = acceptEncoding(Encoding.Gzip)

  def acceptDeflate: Request = acceptEncoding(Encoding.Deflate)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header(name: String, value: String): Request = copy(headers = headers + (name -> List(value)))

  def header(name: String, value: List[String]): Request = copy(headers = headers + (name -> value))



  // - Authentication --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def auth(user: String, pwd: String): Request =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

