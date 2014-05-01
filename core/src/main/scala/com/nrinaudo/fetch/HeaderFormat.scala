package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import scala.util.Try
import java.nio.charset.Charset

trait HeaderReader[T] {
  def read(value: String): Option[T]
}

trait HeaderWriter[T] {
  def write(value: T): String
}

trait HeaderFormat[T] extends HeaderReader[T] with HeaderWriter[T]

object HeaderFormat {
  private class Aggregated[T](reader: HeaderReader[T], writer: HeaderWriter[T]) extends HeaderFormat[T] {
    override def read(value: String): Option[T] = reader.read(value)
    override def write(value: T): String = writer.write(value)
  }

  def format[T](implicit reader: HeaderReader[T], writer: HeaderWriter[T]): HeaderFormat[T] =
    new Aggregated(reader, writer)

  def seqFormat[T: HeaderFormat]: HeaderFormat[Seq[T]] =
    new Aggregated(seqReader(implicitly[HeaderReader[T]]), seqWriter(implicitly[HeaderWriter[T]]))

  implicit def seqWriter[T: HeaderWriter]: HeaderWriter[Seq[T]] = new HeaderWriter[Seq[T]] {
    override def write(value: Seq[T]): String = value.map(implicitly[HeaderWriter[T]].write).mkString(",")
  }

  implicit def seqReader[T: HeaderReader]: HeaderReader[Seq[T]] = new HeaderReader[Seq[T]] {
    override def read(value: String): Option[Seq[T]] =
      value.split(',').map(implicitly[HeaderReader[T]].read).foldRight(Some(Nil): Option[List[T]]) { (value, acc) =>
        (acc, value) match {
          case (Some(list), Some(v)) => Some(v :: list)
          case _                     => None
        }
      }
  }

  /** Formats dates to the proper RFC compliant syntax. */
  implicit object DateFormat extends HeaderFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Option[Date] = HttpDateFormat.synchronized {Try {HttpDateFormat.parse(str)}.toOption}
    override def write(date: Date): String = HttpDateFormat.synchronized {HttpDateFormat.format(date)}

  }

  private val LanguagePattern = """([^-]+)(?:-([^-]+))?""".r
  implicit object LanguageFormat extends HeaderFormat[Locale] {
    override def read (value: String): Option[Locale] = value match {
      case LanguagePattern(lang, null)    => Some(new Locale(lang))
      case LanguagePattern(lang, country) => Some(new Locale(lang, country))
      case _                              => None
    }

    override def write(value: Locale): String = {
      var v = value.getLanguage
      if(!value.getCountry.isEmpty) v += "-" + value.getCountry
      v
    }
  }

  implicit object CharsetFormat extends HeaderFormat[Charset] {
    override def read(value: String): Option[Charset] = Try {Charset.forName(value)}.toOption
    override def write(value: Charset): String = value.name()
  }

  implicit object EncodingFormat extends HeaderFormat[Encoding] {
    override def read(value: String): Option[Encoding] = Encoding.DefaultEncodings.get(value)
    override def write(value: Encoding): String = value.name
  }

  implicit object MimeTypeFormat extends HeaderFormat[MimeType] {
    override def read(str: String): Option[MimeType] = MimeType.unapply(str)
    override def write(t: MimeType): String = t.toString
  }

  implicit object StringFormat extends HeaderFormat[String] {
    override def read(str: String): Option[String] = Some(str)
    override def write(t: String): String = t
  }

  implicit object ETagFormat extends HeaderFormat[ETag] {
    override def read(value: String): Option[ETag] = ETag.unapply(value)
    override def write(value: ETag): String = value.toString
  }

  implicit object ByteRangeFormat extends HeaderFormat[ByteRange] {
    override def read(value: String): Option[ByteRange] = ByteRange.unapply(value)
    override def write(value: ByteRange): String = value.toString
  }

  implicit object ByteRangesFormat extends HeaderFormat[Seq[ByteRange]] {
    private val reader = seqReader[ByteRange]

    override def read(value: String): Option[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           None
    override def write(value: Seq[ByteRange]): String = {
      val builder = new StringBuilder("bytes=")
      var first   = true

      value.foreach { range =>
        if(first) first = false
        else      builder.append(',')
        builder.append(ByteRangeFormat.write(range))
      }
      builder.result()
    }
  }
}