package com.nrinaudo.fetch

object MimeTypeParameters {
  implicit val CharsetFormat = ValueFormat.Charsets

  /** Used to split a MIME parameter into a name and value strings. */
  private val ParamPattern = """([\p{ASCII}&&[^\s]]+)\s*=\s*"?\s*([\p{ASCII}&&[^\s"]]+)\s*"?""".r


  /** Extracts a single MIME parameter. */
  private def param(str: String) = str.trim match {
    case ParamPattern(name, value) => Some(name -> value)
    case _                         => None
  }

  def unapply(str: String): Option[MimeTypeParameters] = {
    if(str == null)Some(new MimeTypeParameters())
    else str.split(";").filter(!_.isEmpty).map(param).foldLeft(Some(new MimeTypeParameters()): Option[MimeTypeParameters]) {
      case (a, p) => for {
        va <- a
        vp <- p
      } yield va.set(vp._1, vp._2)
    }
  }

  def apply(str: String): MimeTypeParameters = unapply(str).getOrElse {
    throw new IllegalArgumentException("Not a valid MIME type parameter list: " + str)
  }
}

class MimeTypeParameters(override val values: Map[String, String] = Map()) extends Parameters[MimeTypeParameters] {
  override def build(values: Map[String, String]): MimeTypeParameters = new MimeTypeParameters(values)

  def writeTo(builder: StringBuilder): StringBuilder = {
    values.foreach { case (name, value) => builder.append(';').append(name).append('=').append(value) }
    builder
  }

  override lazy val toString = writeTo(new StringBuilder()).result()
}