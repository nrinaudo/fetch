package com.nrinaudo.fetch

import java.nio.charset.Charset
import MimeTypeParameters._

object MimeType {
  trait Grammar extends HttpGrammar {
    def component: Parser[String] = "*" | token
    def rawType: Parser[(String, String)] = (component <~ "/") ~ component ^^ { case (main ~ sub) => (main, sub) }

    def mimeType: Parser[MimeType] = rawType ~ opt(paramSep ~> parameters) ^^ {
      case (main, sub) ~ None         => MimeType(main, sub)
      case (main, sub) ~ Some(params) => MimeType(main, sub, new MimeTypeParameters(params))
    }
  }

  private object Format extends Grammar {
    def apply(string: String): Option[MimeType] =
      parseAll(mimeType, string).map(Some(_)).getOrElse(None)
  }

  val TextPlain             = MimeType("text", "plain")
  val ApplicationOctetSteam = MimeType("application", "octet-stream")
  val Json                  = MimeType("application", "json")

  def parse(str: String): Option[MimeType] = Format(str)
}

case class MimeType(main: String, sub: String, params: MimeTypeParameters = new MimeTypeParameters()) {
  def params(values: MimeTypeParameters): MimeType = copy(params = values)

  def removeParam(name: String): MimeType = copy(params = params.remove(name))

  def param[T: ValueWriter](name: String, value: T): MimeType = params(params.set(name, value))

  def param[T: ValueReader](name: String): Option[T] = params.getOpt[T](name)

  def charset: Option[Charset] = param[Charset]("charset")

  def charset(charset: Charset): MimeType = param("charset", charset)

  override lazy val toString = {
    val builder = new StringBuilder(main).append('/').append(sub)
    if(!params.values.isEmpty)
      params.writeTo(builder.append(';'))
    builder.result()
  }
}