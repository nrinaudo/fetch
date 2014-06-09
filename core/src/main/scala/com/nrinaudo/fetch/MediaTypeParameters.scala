package com.nrinaudo.fetch

object MediaTypeParameters {
  implicit val CharsetFormat = ValueFormat.Charsets

  private object Format extends HttpGrammar {
    def apply(string: String): Option[MediaTypeParameters] =
      parseAll(parameters, string).map { params => Some(new MediaTypeParameters(params)) }.getOrElse(None)
  }

  def parse(str: String): Option[MediaTypeParameters] = {
    if(str == null) Some(new MediaTypeParameters())
    else            Format(str)
  }
}

class MediaTypeParameters(override val values: Map[String, String] = Map()) extends Parameters[MediaTypeParameters] {
  import MediaTypeParameters._

  override def build(values: Map[String, String]): MediaTypeParameters = new MediaTypeParameters(values)

  override lazy val toString = Format.parameters(values)
}