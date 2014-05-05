package com.nrinaudo.fetch

import java.text.DecimalFormat
import scala.util.{Failure, Try}
import java.nio.charset.Charset
import java.util.Locale

/** Collection of implicit header formats for known content negotiation headers. */
object Conneg {
  implicit val ConnegEncoding: HeaderFormat[Conneg[Encoding]] = new ConnegFormat[Encoding]
  implicit val ConnegMimeType: HeaderFormat[Conneg[MimeType]] = new ConnegFormat[MimeType]
  implicit val ConnegCharset: HeaderFormat[Conneg[Charset]]   = new ConnegFormat[Charset]
  implicit val ConnegLanguage: HeaderFormat[Conneg[Locale]]   = new ConnegFormat[Locale]
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

private class ConnegFormat[T: HeaderFormat] extends HeaderFormat[Conneg[T]] {
  import ConnegFormat._

  override def read(value: String): Try[Conneg[T]] = value match {
    case ConnegPattern(data, qPattern(q)) => implicitly[HeaderFormat[T]].read(data).map(Conneg(_, q))
    case _                                => Failure(new IllegalArgumentException("Illegal content negotiation header: " + value))
  }

  override def write(value: Conneg[T]): String = {
    val raw = implicitly[HeaderFormat[T]].write(value.value)
    if(value.q == 1)  raw
    else              raw + ";q=" + qFormat.format(value.q)
  }
}

