package com.nrinaudo.fetch

import java.nio.charset.Charset
import MimeTypeParameters._

object MimeType {
  /** Used to split a MIME type string into a main, sub and params strings. */
  private val MimePattern  = """([\w+-]+)/([\w+-]+)\s*(?:;(.*))?""".r


  val TextPlain             = MimeType("text", "plain")
  val ApplicationOctetSteam = MimeType("application", "octet-stream")
  val Json                  = MimeType("application", "json")

  def unapply(str: String): Option[MimeType] = str match {
    case MimePattern(main, sub, MimeTypeParameters(ps)) => Some(new MimeType(main, sub, ps))
    case _                                              => None
  }

  def apply(str: String): MimeType = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal MIME Type: " + str)
  }
}

case class MimeType(main: String, sub: String, params: MimeTypeParameters = new MimeTypeParameters()) {
  def clearParams: MimeType = params(new MimeTypeParameters())

  def params(values: MimeTypeParameters): MimeType = copy(params = values)

  def param[T: ValueWriter](name: String, value: T): MimeType = params(params.set(name, value))

  def param[T: ValueReader](name: String): Option[T] = params.getOpt[T](name)

  def charset: Option[Charset] = param[Charset]("charset")

  def charset(charset: Charset): MimeType = param("charset", charset)

  override lazy val toString = params.writeTo(new StringBuilder(main).append('/').append(sub)).result()
}