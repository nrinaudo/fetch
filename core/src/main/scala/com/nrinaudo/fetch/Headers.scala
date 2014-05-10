package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import scala.util.{Failure, Success, Try}
import java.nio.charset.Charset

object Headers {
  type HeaderReader[T] = ValueReader[String, T]
  type HeaderWriter[T] = ValueWriter[T, String]
  type HeaderFormat[T] = ValueFormat[String, T]


  // - Composite header formats ----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def compositeFormat[T: HeaderFormat]: HeaderFormat[Seq[T]] =
    ValueFormat(compositeReader(implicitly[HeaderReader[T]]), compositeWriter(implicitly[HeaderWriter[T]]))

  implicit def compositeWriter[T: HeaderWriter]: HeaderWriter[Seq[T]] =
    ValueWriter.seq(implicitly[HeaderWriter[T]]).andThen {s => Some(s.mkString(","))}

  implicit def compositeReader[T: HeaderReader]: HeaderReader[Seq[T]] =
    ValueReader((s: String) => Success(s.split(',').toSeq)).andThen(ValueReader.seq(implicitly[HeaderReader[T]]))


  // - Generic default formats -----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val StringFormat: HeaderFormat[String] = KeyValueStore.StringFormats.Strings
  implicit val IntFormat: HeaderFormat[Int]       = KeyValueStore.StringFormats.Ints


  // - Header specific default formats ---------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Formats dates to the proper RFC compliant syntax. */
  implicit object DateFormat extends HeaderFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Try[Date]      = HttpDateFormat.synchronized {Try {HttpDateFormat.parse(str)}}
    override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
  }

  private val LanguagePattern = """([^-]+)(?:-([^-]+))?""".r
  implicit object LanguageFormat extends HeaderFormat[Locale] {
    override def read (value: String): Try[Locale] = value match {
      case LanguagePattern(lang, null)    => Success(new Locale(lang))
      case LanguagePattern(lang, country) => Success(new Locale(lang, country))
      case _                              => Failure(new IllegalArgumentException("Not a valid locale: " + value))
    }

    override def write(value: Locale): Option[String] = {
      var v = value.getLanguage

      if(!value.getCountry.isEmpty) v += "-" + value.getCountry

      Some(v)
    }
  }

  implicit object CharsetFormat extends HeaderFormat[Charset] {
    override def read(value: String): Try[Charset]     = Try {Charset.forName(value)}
    override def write(value: Charset): Option[String] = Some(value.name())
  }

  implicit object EncodingFormat extends HeaderFormat[Encoding] {
    override def read(value: String): Try[Encoding] =
      Encoding.DefaultEncodings.get(value) map {Success(_)} getOrElse Failure(new IllegalArgumentException("Unsupported encoding: " + value))
    override def write(value: Encoding): Option[String] = Some(value.name)
  }

  implicit object MimeTypeFormat extends HeaderFormat[MimeType] {
    override def read(str: String): Try[MimeType]   = Try {MimeType(str)}
    override def write(t: MimeType): Option[String] = Some(t.toString)
  }

  implicit object MethodFormat extends HeaderFormat[Method] {
    override def read(value: String): Try[Method] = Method.unapply(value) map {Success(_)} getOrElse {
      throw new IllegalArgumentException("Unsupported method: " + value)
    }

    override def write(value: Method): Option[String] = Some(value.name)
  }

  implicit object ETagFormat extends HeaderFormat[ETag] {
    override def read(value: String): Try[ETag]     = Try {ETag(value)}
    override def write(value: ETag): Option[String] = Some(value.toString)
  }

  implicit object ByteRangeFormat extends HeaderFormat[ByteRange] {
    override def read(value: String): Try[ByteRange]     = Try {ByteRange(value)}
    override def write(value: ByteRange): Option[String] = Some(value.toString)
  }

  implicit object ByteRangesFormat extends HeaderFormat[Seq[ByteRange]] {
    private val reader = compositeReader[ByteRange]
    private val writer = compositeWriter[ByteRange]

    override def read(value: String): Try[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           Failure(new IllegalArgumentException("Illegal byte ranges: " + value))

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }
}

class Headers(override val values: Map[String, String] = Map()) extends KeyValueStore[String, Headers] {
  override def build(values: Map[String, String]): Headers = new Headers(values)
}