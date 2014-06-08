package com.nrinaudo.fetch

import java.util.Date
import Headers._

/**
 * Represents an HTTP response.
 */
case class Response[A](status: Status, headers: Headers, body: A) {
  // - Monadic operations ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def map[B](f: A => B): Response[B] = copy(body = f(body))
  def flatMap[B](f: A => Response[B]): Response[B] = copy(body = f(body).body)
  def foreach[U](f: A => U): U = f(body)



  // - Header helpers ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def date: Option[Date] = headers.getOpt[Date]("Date")
  def contentEncoding: Option[Seq[Encoding]] = headers.getOpt[Seq[Encoding]]("Content-Encoding")
  def contentLanguage: Option[Seq[Language]] = headers.getOpt[Seq[Language]]("Content-Language")
  def contentType: Option[MediaType] = headers.getOpt[MediaType]("Content-Type")
  def lastModified: Option[Date] = headers.getOpt[Date]("Last-Modified")
  def expires: Option[Date] = headers.getOpt[Date]("Expires")
  def etag: Option[ETag] = headers.getOpt[ETag]("ETag")
  def server: Option[String] = headers.getOpt[String]("Server")
  def allow: Option[Seq[Method]] = headers.getOpt[Seq[Method]]("Allow")
  def age: Option[Int] = headers.getOpt[Int]("Age")
  def wwwAuthenticate: Option[String] = headers.getOpt[String]("WWW-Authenticate")

  // TODO: implement Accept-Ranges
  // TODO: implement Cache-Control
  // TODO: implement Connection
  // TODO: implement Content-Length
  // TODO: implement Content-Location
  // TODO: implement Content-Disposition
  // TODO: implement Content-Range
  // TODO: implement Location
  // TODO: implement Pragma
  // TODO: implement Retry-After
  // TODO: implement Set-Cookie
  // TODO: implement Vary
}