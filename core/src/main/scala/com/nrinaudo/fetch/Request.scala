package com.nrinaudo.fetch

import org.apache.commons.codec.binary.Base64
import java.io.OutputStream
import java.util.zip._
import scala.Some

/**
 * @author Nicolas Rinaudo
 */
case class Request(url:     java.net.URL,
                   method:  String                = "GET",
                   body:    Option[RequestEntity] = None,
                   headers: Headers               = Map()) {
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

  /** Compresses the request entity using the specified algorithm.
    * Note that not all servers support all compression algorithms. Perform an OPTION call on the target resource if
    * unsure.
    * @see http://tools.ietf.org/html/rfc2616#section-3.5
    */
  def contentEncoding(name: String, f: OutputStream => DeflaterOutputStream) =
    body map {entity =>
      header("Content-Encoding", name).body(new DeflatedEntity(entity, f))
    } getOrElse {throw new IllegalStateException("Cannot specify a content encoding for a request without entity body")}

  /** Compresses the request entity using GZIP. */
  def gzip = contentEncoding("gzip", (out) => new GZIPOutputStream(out))

  /** Compresses the request entity using the deflate compression mechanism. */
  def deflate = contentEncoding("deflate", (out) => new DeflaterOutputStream(out))



  // - Generic headers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def header(name: String, value: String) = copy(headers = headers + (name -> List(value)))

  def header(name: String, value: List[String]) = copy(headers = headers + (name -> value))



  // - Authentication --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def auth(user: String, pwd: String) =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))
}

