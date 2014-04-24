package com.nrinaudo.fetch

import java.text.DecimalFormat
import scala.util.Try
import java.nio.charset.Charset
import java.util.Locale
import Headers._

object Conneg {
  implicit val ConnegEncoding: HeaderFormat[Conneg[Encoding]] = new ConnegFormat[Encoding]
  implicit val ConnegMimeType: HeaderFormat[Conneg[MimeType]] = new ConnegFormat[MimeType]
  implicit val ConnegCharset: HeaderFormat[Conneg[Charset]]   = new ConnegFormat[Charset]
  implicit val ConnegLocale: HeaderFormat[Conneg[Locale]]   = new ConnegFormat[Locale]
}

/** Represents an acceptable value for content negotiation headers (`Accept-*`).
  *
  * @param  value value of the header (its `toString` method will be used when setting HTTP headers).
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
    def unapply(str: String): Option[Float] = Try {str.toFloat}.toOption.filter {q => q >= 0 && q <= 1}
  }
}

private class ConnegFormat[T: HeaderFormat] extends HeaderFormat[Conneg[T]] {
  import ConnegFormat._

  override def parse(value: String): Conneg[T] = value match {
    case ConnegPattern(data, qPattern(q)) => Conneg(implicitly[HeaderFormat[T]].parse(data), q)
    case _                                => throw new IllegalArgumentException("Illegal conneg value: " + value)
  }

  override def format(value: Conneg[T]): String = {
    val raw = implicitly[HeaderFormat[T]].format(value.value)
    if(value.q == 1)  raw
    else              raw + ";q=" + qFormat.format(value.q)
  }
}

