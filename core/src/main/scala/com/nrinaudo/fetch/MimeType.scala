package com.nrinaudo.fetch

import java.nio.charset.Charset

// TODO: provide better ways to specify MIME type parameters (ValueReader / ValueWriter).
// TODO: unapply is broken in that an illegal parameter will cause an exception to be thrown rather than None to be returned.

object MimeType {
  private val TSpecials= Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', ',', '[', ']', '?', '=')

  /** Used to split a MIME type string into a main, sub and params strings. */
  private val MimePattern  = """([\w+-]+)/([\w+-]+)\s*(?:;(.*))?""".r

  /** Used to split a MIME parameter into a name and value strings. */
  private val ParamPattern = """([\p{ASCII}&&[^\s]]+)\s*=\s*"?\s*([\p{ASCII}&&[^\s"]]+)\s*"?""".r

  /** Extracts a single MIME parameter. */
  private def param(str: String) = str.trim match {
    case ParamPattern(name, value) => name -> value
    case _                         => throw new IllegalArgumentException("Not a valid MIME parameter: " + str)
  }

  /** Extracts a MIME parameters string into a Map[String, String]. */
  private def params(str: String): Map[String, String] = {
    if(str == null) Map()
    else            str.split(";").foldLeft(Map[String, String]()) {case (a, p) => a + param(p)}
  }

  private def paramValue(value: String) =
    if(value.exists(TSpecials)) '\"' + value + '\"'
    else                        value

  val TextPlain             = MimeType("text", "plain")
  val ApplicationOctetSteam = MimeType("application", "octet-stream")
  val Json                  = MimeType("application", "json")

  def unapply(str: String): Option[MimeType] = str match {
    case MimePattern(main, sub, ps) => Some(MimeType(main, sub, params(ps)))
    case _                          => None
  }

  def apply(str: String): MimeType = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal MIME Type: " + str)
  }
}

case class MimeType(main: String, sub: String, params: Map[String, String] = Map()) {
  lazy val charset: Option[Charset] = params.get("charset").map {Charset.forName}

  def charset(c: Charset): MimeType = charset(c.name)

  def charset(c: String): MimeType = copy(params = params + ("charset" -> c))

  override lazy val toString = params.foldLeft(new StringBuilder(main).append('/').append(sub)) {
    case (builder, (name, value)) => builder.append(';').append(name).append('=').append(MimeType.paramValue(value))
  }.result()
}