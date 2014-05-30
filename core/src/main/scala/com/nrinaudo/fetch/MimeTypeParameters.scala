package com.nrinaudo.fetch

object MimeTypeParameters {
  implicit val CharsetFormat = ValueFormat.Charsets

  private object Format extends HttpGrammar {
    def apply(string: String): Option[MimeTypeParameters] =
      parseAll(parameters, string).map { params => Some(new MimeTypeParameters(params)) }.getOrElse(None)
  }

  def unapply(str: String): Option[MimeTypeParameters] = {
    if(str == null) Some(new MimeTypeParameters())
    else            Format(str)
  }

  def apply(str: String): MimeTypeParameters = unapply(str).getOrElse {
    throw new IllegalArgumentException("Not a valid MIME type parameter list: " + str)
  }
}

class MimeTypeParameters(override val values: Map[String, String] = Map()) extends Parameters[MimeTypeParameters] {
  import MimeTypeParameters._

  override def build(values: Map[String, String]): MimeTypeParameters = new MimeTypeParameters(values)

  def writeTo(builder: StringBuilder): StringBuilder = Format.writeParametersTo(builder, values)

  override lazy val toString = Format.parameters(values)
}