package com.nrinaudo.fetch

import fastparse.Parser

object MediaTypeParameters {
  implicit val charsetParam = ValueFormat.charsetParam

  private[fetch] val parser: Parser[MediaTypeParameters] =
    grammar.params.map(p => MediaTypeParameters(p.foldLeft(Map.empty[String, String]) { case (acc, param) => acc + param }))

  def parse(str: String): Option[MediaTypeParameters] =
    if(str == null) Some(MediaTypeParameters.empty)
    else parseFully(parser, str)

  def empty: MediaTypeParameters = MediaTypeParameters(Map.empty)
}

case class MediaTypeParameters(override val values: Map[String, String]) extends Parameters[MediaTypeParameters] {
  override def build(values: Map[String, String]): MediaTypeParameters = new MediaTypeParameters(values)
}