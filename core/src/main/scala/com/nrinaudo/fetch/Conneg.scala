package com.nrinaudo.fetch

import java.text.DecimalFormat
import scala.util.{Failure, Try}
import java.nio.charset.Charset
import java.util.Locale
import com.nrinaudo.fetch.Headers._
import sun.net.www.content.image.png

/** Collection of implicit header formats for known content negotiation headers. */
object Conneg {
  implicit object MimeTypeConneg extends ValueFormat[Conneg[MimeType]] {
    override def write(value: Conneg[MimeType]): Option[String] = Some(value.value.param("q", value.q).toString)
    override def read(value: String): Try[Conneg[MimeType]] = Try {MimeType(value)} map { mime =>
      Conneg(mime.removeParam("q"), mime.param[Float]("q").getOrElse(1f))
    }
  }

  implicit val ConnegEncoding: ValueFormat[Conneg[Encoding]] = new ConnegFormat[Encoding]
  implicit val ConnegCharset: ValueFormat[Conneg[Charset]]   = new ConnegFormat[Charset]
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

