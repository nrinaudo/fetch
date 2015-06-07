package com.nrinaudo.fetch

import fastparse.Parser

object MediaTypeParameters {
  implicit val charsetParam = ValueFormat.charsetParam

  private[fetch] val parser: Parser[Parameters] =
    grammar.params.map(p => Parameters(p.foldLeft(Map.empty[String, String]) { case (acc, param) => acc + param }))

  def parse(str: String): Option[Parameters] =
    if(str == null) Some(Parameters.empty)
    else parseFully(parser, str)
}