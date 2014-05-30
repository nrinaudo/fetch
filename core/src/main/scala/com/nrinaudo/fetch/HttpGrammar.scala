package com.nrinaudo.fetch

import scala.util.parsing.combinator._

object HttpGrammar extends RegexParsers {
  override def skipWhitespace = false
  def token: Parser[String] = """[\u0020-\u007E&&[^ \t()<>@,;:\"/\[\]?={}]]+""".r
  def quotedPair: Parser[String] = "\\" ~> """[\u0000-\u007F]""".r
  def qdtext: Parser[String] = """[\u0000-\u007f&&[^\"\\]]+""".r
  def quotedString: Parser[String] = "\"" ~> (rep(quotedPair | qdtext) ^^ (_.mkString)) <~ "\""
}
