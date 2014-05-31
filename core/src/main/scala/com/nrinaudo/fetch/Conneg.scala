package com.nrinaudo.fetch

import java.text.DecimalFormat
import scala.util.Try
import java.nio.charset.Charset
import java.util.Locale
import com.nrinaudo.fetch.Headers._
import scala.util.Failure

/** Collection of implicit header formats for known content negotiation headers. */
object Conneg {
  /*
  implicit object MimeTypesConneg extends ValueFormat[Seq[Conneg[MimeType]]] with MimeType.Grammar {
    def connegs: Parser[List[Conneg[MimeType]]] = repsep(mimeType, ",") ^^ { list =>
      list.map { mime => Conneg(mime.removeParam("q"), mime.param[Float]("q").getOrElse(1f)) }
    }

    override def write(value: Seq[Conneg[MimeType]]): Option[String] =
      Some(value.map(c => c.value.toString + ";" + qValue(c.q)).mkString(","))

    override def read(value: String): Try[List[Conneg[MimeType]]] =
      parseAll(connegs, value) match {
        case Success(a, _) => scala.util.Success(a)
        case Failure(msg, _) => scala.util.Failure(new IllegalArgumentException(msg))
        case Error(msg, _) => scala.util.Failure(new IllegalArgumentException(msg))
      }
  }
  */


  /** Used to parse and serialize instances of `Conneg[MimeType]`. */
  implicit object MimeTypeConneg extends ValueFormat[Conneg[MimeType]] {
    override def write(value: Conneg[MimeType]): Option[String] = Some(value.value.param("q", value.q).toString)
    override def read(value: String): Try[Conneg[MimeType]] = Try {MimeType(value)} map { mime =>
      Conneg(mime.removeParam("q"), mime.param[Float]("q").getOrElse(1f))
    }
  }

  /** Used to parse and serialize instances of `Conneg[Encoding]`. */
  implicit object EncodingConneg extends ValueFormat[Conneg[Encoding]] with HttpGrammar {
    def encoding: Parser[Encoding] = (Encoding.Gzip.name | Encoding.Deflate.name | Encoding.Identity.name) ^^ {
      case Encoding.Gzip.name     => Encoding.Gzip
      case Encoding.Deflate.name  => Encoding.Deflate
      case Encoding.Identity.name => Encoding.Identity
    }

    def conneg: Parser[Conneg[Encoding]] = encoding ~ (paramSep ~> qValue) ^^ {
      case encoding ~ q => new Conneg(encoding, q)
    }

    override def write(value: Conneg[Encoding]): Option[String] = Some(value.value.name + ";" + qValue(value.q))
    override def read(value: String): Try[Conneg[Encoding]] =
      parseAll(conneg, value) match {
        case Success(a, _)   => scala.util.Success(a)
        case Failure(msg, _) => scala.util.Failure(new IllegalArgumentException(msg))
        case Error(msg, _)   => scala.util.Failure(new IllegalArgumentException(msg))
      }
  }

  // token
  implicit val ConnegCharset: ValueFormat[Conneg[Charset]]   = new ConnegFormat[Charset]

  // char 1-8 - char 1-8
  implicit val ConnegLanguage: ValueFormat[Conneg[Locale]]   = new ConnegFormat[Locale]
}

/** Represents an acceptable value for content negotiation headers (`Accept*`).
  *
  * @param  value value of the header (see the companion object for header formats).
  * @param  q     weight of the value, as a float between 0 and 1 inclusive. Bigger weights tell remote servers that
  *               the corresponding value is more desirable than values associated with lower weights.
  */
case class Conneg[T](value: T, q: Float = 1) {
  require(q >= 0 && q <= 1, "q must be between 0 and 1, inclusive.")

  def map[S](f: T => S): Conneg[S] = Conneg(f(value), q)
  def flatMap[S](f: T => Conneg[S]): Conneg[S] = f(value).copy(q = q)
}

private object ConnegFormat {
  /** Format for the `q` content-negotiation header. */
  val qFormat = new DecimalFormat("0.###")

  val ConnegPattern = """([^;]+)(?:;\s*q\s*=\s*([0-9.]+))?""".r

  object qPattern {
    def unapply(str: String): Option[Float] =
      if(str == null) Some(1.0f)
      else            Try {str.toFloat}.toOption.filter {q => q >= 0 && q <= 1}
  }
}

private class ConnegFormat[T: ValueFormat] extends ValueFormat[Conneg[T]] {
  import ConnegFormat._

  override def read(value: String): Try[Conneg[T]] = value match {
    case ConnegPattern(data, qPattern(q)) => implicitly[ValueFormat[T]].read(data).map(Conneg(_, q))
    case _                                => Failure(new IllegalArgumentException("Illegal content negotiation header: " + value))
  }

  override def write(value: Conneg[T]): Option[String] = {
    val raw = implicitly[ValueFormat[T]].write(value.value)
    if(value.q == 1)  raw
    else              raw map(_ + ";q=" + qFormat.format(value.q))
  }
}

