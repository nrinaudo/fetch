package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import scala.Some

/**
 * @author Nicolas Rinaudo
 */
case class Request(url:     java.net.URL,
                   method:  String                = "GET",
                   body:    Option[RequestEntity] = None,
                   headers: Headers               = Map(),
                   encoding: Encoding             = Encoding.Identity) {
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



  // - Entity body manipulation ----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def body(entity: RequestEntity) = copy(body = Some(entity))

  /** Encodes the request using the specified encoding. */
  def encode(encoding: Encoding) = copy(encoding = encoding)

  /** Compresses the request entity using GZIP. */
  def gzip = encode(Encoding.Gzip)

  /** Compresses the request entity using the deflate compression mechanism. */
  def deflate = encode(Encoding.Deflate)



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header(name: String, value: String) = copy(headers = headers + (name -> List(value)))

  def header(name: String, value: List[String]) = copy(headers = headers + (name -> value))



  // - Authentication --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def auth(user: String, pwd: String) =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

