package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import com.nrinaudo.fetch.Request.Engine
import java.net.URL

object Request {
  type Engine = (URL, String, Option[RequestEntity], Headers) => Response[ResponseEntity]

  def apply(url: URL)(implicit engine: Engine): Request = Request(engine, url)
}

/**
 * @author Nicolas Rinaudo
 */
case class Request(engine:  Engine,
                   url:     URL,
                   method:  String  = "GET",
                   headers: Headers = Map()) extends (Option[RequestEntity] => Response[ResponseEntity]) {
  // - Execution -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def apply(body: Option[RequestEntity]): Response[ResponseEntity] = {
    var h = headers

    // Sets body specific HTTP headers.
    body foreach {b =>
      h = h + ("Content-Type" -> List(b.mimeType.toString))
      if(b.encoding == Encoding.Identity) h = h - "Content-Encoding"
      else                                h = h + ("Content-Encoding" -> List(b.encoding.name))
    }

    engine(url, method, body, h)
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



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header(name: String, value: String): Request = copy(headers = headers + (name -> List(value)))

  def header(name: String, value: List[String]): Request = copy(headers = headers + (name -> value))



  // - Authentication --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def auth(user: String, pwd: String): Request =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

