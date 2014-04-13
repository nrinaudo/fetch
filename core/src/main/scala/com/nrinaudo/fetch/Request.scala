package com.nrinaudo.fetch

import java.nio.charset.Charset
import org.apache.commons.codec.binary.Base64

/**
 * @author Nicolas Rinaudo
 */
case class Request(url: java.net.URL,
                   method: String               = "GET",
                   body: Option[RequestEntity]  = None,
                   charset: Option[Charset]     = None,
                   headers: Map[String, String] = Map()) {
  // - HTTP methods ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def method(method: String): Request = copy(method = method)
  def GET: Request     = method("GET")
  def POST: Request    = method("POST")
  def PUT: Request     = method("PUT")
  def DELETE: Request  = method("DELETE")
  def HEAD: Request    = method("HEAD")
  def OPTIONS: Request = method("OPTIONS")
  def TRACE: Request   = method("TRACE")
  def CONNECT: Request = method("CONNECT")
  def PATCH: Request   = method("PATCH")
  def LINK: Request    = method("LINK")
  def UNLINK: Request  = method("UNLINK")

  def body(entity: RequestEntity) = copy(body = Some(entity))

  def auth(user: String, pwd: String) =
    header("Authorization", "Basic " + Base64.encodeBase64String((user + ':' + pwd).getBytes))

  def charset(name: String): Request = charset(Charset.forName(name))
  def charset(value: Charset): Request = copy(charset = Some(value))

  def header(name: String, value: String) = copy(headers = headers + (name -> value))
}
