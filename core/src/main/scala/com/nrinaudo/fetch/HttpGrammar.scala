package com.nrinaudo.fetch

import scala.util.parsing.combinator._

object HttpGrammar {
  val Separators = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{', '}', ' ', '\t')
}

trait HttpGrammar extends RegexParsers {
  import HttpGrammar._

  override def skipWhitespace = false

  // - Base elements ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def token: Parser[String]        = """[\u0020-\u007E&&[^ \t()<>@,;:\"/\[\]?={}]]+""".r
  def quotedPair: Parser[String]   = "\\" ~> """[\u0000-\u007F]""".r
  def qdtext: Parser[String]       = """[\u0000-\u007f&&[^\"\\]]+""".r
  def quotedString: Parser[String] = "\"" ~> (rep(quotedPair | qdtext) ^^ (_.mkString)) <~ "\""
  def content: Parser[String]      = quotedString | token
  def content(value: String): String =
    if(value.exists(mustEscape)) "\"%s\"" format value.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"")
    else                         value

  private def mustEscape(c: Char) = c < '\u0020' || c > '\u007E' || Separators.contains(c)

  // - Parameters ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def valueSep: Parser[Any] = """\s*=\s*""".r
  def paramSep: Parser[Any] = """\s*;\s*""".r


  def parameter: Parser[(String, String)] = (token ~ (valueSep ~> (token | quotedString))) ^^ {
    case token ~ value => (token, value)
  }

  def parameters: Parser[Map[String, String]] = repsep(parameter, paramSep) ^^ {_.foldLeft(Map[String, String]()) {
    case (params, param) => params + param
  }}

  def writeParametersTo(builder: StringBuilder, params: Map[String, String]): StringBuilder = {
    var first = true

    params.foreach { case (name, value) =>
      if(first) first = false
      else      builder.append(';')
      builder.append(name).append('=').append(content(value)) }
    builder
  }
  def parameters(params: Map[String, String]): String = writeParametersTo(new StringBuilder(), params).result()
}
