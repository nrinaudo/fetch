package com.nrinaudo.fetch

import java.text.DecimalFormat
import java.nio.charset.Charset
import com.nrinaudo.fetch.Headers._

/** Collection of implicit header formats for known content negotiation headers. */
object Conneg {
  /** Implicit format for the `Accept` content negotiation header. */
  implicit object MimeTypes extends MimeType.Grammar with ConnegFormat[MimeType] {
    override def entry: Parser[MimeType] = mimeType

    // This is slightly different from other formats: mime types accept parameter in the exact same syntax as the q
    // parameter.
    override def connegs: Parser[List[Conneg[MimeType]]] = repsep(mimeType, ",") ^^ { list =>
      list.map { mime => Conneg(mime.removeParam("q"), mime.param[Float]("q").getOrElse(1f)) }
    }
  }

  /** Implicit format for the `Accept-Encoding` content negotiation header.
    *
    * In its current version, this only supports known transfer encodings (`gzip`, `deflate` and `identity`).
    */
  implicit object Encodings extends ConnegFormat[Encoding] {
    override def entry: Parser[Encoding] = (Encoding.Gzip.name | Encoding.Deflate.name | Encoding.Identity.name) ^^ {
      case Encoding.Gzip.name     => Encoding.Gzip
      case Encoding.Deflate.name  => Encoding.Deflate
      case Encoding.Identity.name => Encoding.Identity
    }
  }

  /** Implicit format for the `Accept-Charset` content negotiation header. */
  // TODO: check what happens when a valid token that does not map to a supported charset is passed.
  implicit object Charsets extends ConnegFormat[Charset] {
    override def entry: Parser[Charset] = token ^^ Charset.forName
  }

  /** Implicit format for the `Accept-Language` content negotiation header. */
  implicit object Languages extends ConnegFormat[Language] with Language.Grammar {
    override def entry: Parser[Language] = language
  }
}

/** Used to parse / serialize content negotiation header values.
  *
  * In order to implement a format for a specific type, application developers only need to provide a `Parser`
  * instance for that type by implementing the [[entry]] method.
  */
trait ConnegFormat[T] extends HttpGrammar with ValueFormat[Seq[Conneg[T]]] {
  private val qFormat = new DecimalFormat("0.###")

  /** Parser for the underlying type. */
  def entry: Parser[T]

  /** Parser for a single conneg value. */
  def conneg: Parser[Conneg[T]] = entry ~ opt(paramSep ~> qValue) ^^ {
    case entry ~ Some(q) => new Conneg(entry, q)
    case entry ~ _       => new Conneg(entry, 1.0f)
  }

  /** Parser for a list of conneg values. */
  def connegs: Parser[List[Conneg[T]]] = repsep(conneg, ",")

  /** Parser for the content-negotiation `q` parameter. */
  def qValue: Parser[Float] = ("q" ~ valueSep) ~> """[0-1](\.[0-9]{1,3})?""".r ^^ (_.toFloat)

  /** Formats the content-negotiation `q` parameter. */
  def qValue(value: Float): String = "q=%s".format(qFormat.format(value))

  private def writeEntry(entry: Conneg[T]): String =
    if(entry.q == 1) entry.value.toString
    else             entry.value.toString + ";" + qValue(entry.q)

  // TODO: this currently relies on toString, change that.
  override def write(value: Seq[Conneg[T]]): Option[String] = {
    if(value.isEmpty) None
    else              Some(value.map(writeEntry).mkString(","))
  }

  override def read(value: String): Option[Seq[Conneg[T]]] = parseAll(connegs, value) match {
    case Success(a, _) => Some(a)
    case _             => None
  }
}

/** Represents an acceptable value for content negotiation headers (`Accept*`).
  *
  * @param  value value of the header (see the companion object for header formats).
  * @param  q     weight of the value, as a float between 0 and 1 inclusive. Bigger weights tell remote servers that
  *               the corresponding value is more desirable than values associated with lower weights.
  */
case class Conneg[T](value: T, q: Float = 1) {
  require(q >= 0 && q <= 1, "q must be between 0 and 1, inclusive.")
}

