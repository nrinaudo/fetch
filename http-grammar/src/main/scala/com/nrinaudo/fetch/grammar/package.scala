package com.nrinaudo.fetch

import java.text.DecimalFormat

import fastparse._

/** Implements parsers for the various elements of the HTTP grammar as defined in [[http://tools.ietf.org/html/rfc2616 RFC 2616]]. */
package object grammar {
  // - Character predicates --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** List of separator characters. */
  val Separators = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{', '}', ' ', '\t')

  def isChar(c: Char): Boolean = c >= 0 && c <= 127
  def isSeparator(c: Char): Boolean = Separators.contains(c)
  def isCtl(c: Char): Boolean = (c >= 0 && c <= 31) || c == 127
  def isLws(c: Char): Boolean = c == '\r' || c == '\n' || c == ' ' || c == '\t'
  def isText(c: Char): Boolean = !isCtl(c) || isLws(c)

  val token: Parser[String] = CharsWhile(c => isChar(c) && !(isCtl(c) || isSeparator(c)), 1).!
  // Note: I've added the \\ exclusion here because while the RFC does not specify it, it seems that it should be there
  // to prevent conflict with quoted pairs.
  val qdtext: Parser[String] = CharsWhile(c => isText(c) &&  c != '"' && c != '\\', 1).!
  val quotedPair: Parser[String] = "\\" ~ CharPred(isChar).!
  val quotedString: Parser[String] = "\"" ~ (quotedPair | qdtext).rep.map(_.mkString) ~ "\""

  val param: Parser[(String, String)] = token ~ "=" ~ (token | quotedString).?.map(_.getOrElse(""))
  val params: Parser[Map[String, String]] = param.rep(0, ";").map(_.foldLeft(Map.empty[String, String])(_ + _))

  val mediaType: Parser[(String, String, Seq[(String, String)])] = token ~ "/" ~ token ~ (";" ~ params).?.map(_.getOrElse(List.empty).toSeq)

  // TODO: both languageTag and qValue are incorrect: they do not check for max size.
  val languageTag: Parser[String] = P(CharIn('a' to 'z', 'A' to 'Z')).rep(1).!
  val language: Parser[(String, Seq[String])] = languageTag ~ ("-" ~ languageTag).rep

  val qValue: Parser[Float] = (P("0" ~ ("." ~ CharsWhile('0' to '9' contains _)).?) |
                               P("1" ~ ("." ~ "0".rep(1)).?)).!.map(_.toFloat)
  def conneg[T](parser: Parser[T]): Parser[(T, Float)] = parser ~ (";q=" ~ qValue).?.map(_.getOrElse(1.0F))
  def connegs[T](parser: Parser[T]): Parser[Seq[(T, Float)]] = conneg(parser).rep(0, ",")



  // - Serialization ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private def mustEscape(c: Char) = c < '\u0020' || c > '\u007E' || Separators.contains(c)

  def quotedString(str: String): String = "\"" + str.replaceAllLiterally("\\", "\\\\").replaceAllLiterally("\"", "\\\"") + "\""

  def content(value: String): String =
    if(value.exists(mustEscape)) quotedString(value)
    else                         value

  private def param(name: String, value: String, out: StringBuilder): StringBuilder =
    out.append(name).append('=').append(content(value))

  private val qFormat = new DecimalFormat("0.###")
  def qValue(q: Float): String = qFormat.format(q.toDouble)

  def param(name: String, value: String): String = param(name, value, new StringBuilder).result()

  def params(params: Map[String, String]): String = {
    val builder = new StringBuilder
    var first = true

    params.foreach { case (name, value) =>
      if(first) first = false
      else builder.append(';')
      param(name, value, builder)
    }

    builder.result()
  }

  private def conneg(value: String, q: Float, out: StringBuilder): StringBuilder =
    if(q >= 1F) out.append(value)
    else        param("q", qValue(q), out.append(value).append(';'))

  def conneg(value: String, q: Float): String = conneg(value, q, new StringBuilder).result()

  def connegs(values: Seq[(String, Float)]): String = {
    val builder = new StringBuilder
    var first = true

    values.foreach { case (name, value) =>
      if(first) first = false
      else builder.append(',')
      conneg(name, value, builder)
    }

    builder.result()
  }

  def language(main: String, sub: Seq[String]): String =
    sub.foldLeft(new StringBuilder(main))(_.append('-').append(_)).result()
}
