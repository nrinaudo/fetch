package com.nrinaudo.fetch

object MediaTypeParameters {
  implicit val CharsetFormat = ValueFormat.charsetParam

  private object Format extends HttpGrammar {
    def apply(string: String): Option[MediaTypeParameters] =
      parseAll(parameters, string).map { params => Some(new MediaTypeParameters(params)) }.getOrElse(None)
  }

  def parse(str: String): Option[MediaTypeParameters] = {
    if(str == null) Some(MediaTypeParameters.empty)
    else            Format(str)
  }

  def empty: MediaTypeParameters = MediaTypeParameters(Map.empty)
}

case class MediaTypeParameters(override val values: Map[String, String]) extends Parameters[MediaTypeParameters] {
  import MediaTypeParameters._

  override def build(values: Map[String, String]): MediaTypeParameters = new MediaTypeParameters(values)

  override lazy val toString = Format.parameters(values)
}