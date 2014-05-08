package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import scala.util.{Failure, Success, Try}
import java.nio.charset.Charset

// - Reader ------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object HeaderReader {
  def apply[T](f: String => Try[T]) = new HeaderReader[T] {
    override def read(value: String): Try[T] = f(value)
  }
}

trait HeaderReader[T] extends ValueReader[String, T]



// - Writer ------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object HeaderWriter {
  def apply[T](f: T => Option[String]) = new HeaderWriter[T] {
    override def write(value: T): Option[String] = f(value)
  }
}

trait HeaderWriter[T] extends ValueWriter[T, String]



// - Format ------------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait HeaderFormat[T] extends HeaderReader[T] with HeaderWriter[T]

object HeaderFormat {
  private class Aggregated[T](reader: HeaderReader[T], writer: HeaderWriter[T]) extends HeaderFormat[T] {
    override def read(value: String): Try[T] = reader.read(value)
    override def write(value: T): Option[String] = writer.write(value)
  }

  def apply[T](reader: String => Try[T], writer: T => Option[String]): HeaderFormat[T] =
    apply(HeaderReader(reader), HeaderWriter(writer))

  def apply[T](implicit reader: HeaderReader[T], writer: HeaderWriter[T]): HeaderFormat[T] =
    new Aggregated(reader, writer)

  def seqFormat[T: HeaderFormat]: HeaderFormat[Seq[T]] =
    new Aggregated(seqReader(implicitly[HeaderReader[T]]), seqWriter(implicitly[HeaderWriter[T]]))

  implicit def seqWriter[T: HeaderWriter]: HeaderWriter[Seq[T]] = new HeaderWriter[Seq[T]] {
    override def write(value: Seq[T]): Option[String] = {
      value.map(implicitly[HeaderWriter[T]].write).collect {
        case Some(v) => v
      } match {
        case Nil  => None
        case list => Some(list.mkString(","))
      }
    }
  }

  implicit def seqReader[T: HeaderReader]: HeaderReader[Seq[T]] = new HeaderReader[Seq[T]] {
    override def read(value: String): Try[Seq[T]] =
      value.split(',').map(implicitly[HeaderReader[T]].read).foldRight(Success(Nil): Try[List[T]]) { (value, acc) =>
        (acc, value) match {
          case (Success(list), Success(v)) => Success(v :: list)
          case (Failure(_), _)             => acc
          case (_,             Failure(e)) => Failure(e)
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

  implicit object StringFormat extends HeaderFormat[String] {
    override def read(str: String): Try[String]   = Success(str)
    override def write(t: String): Option[String] =
      if(t.isEmpty) None
      else          Some(t)
  }

  implicit object IntFormat extends HeaderFormat[Int] {
    override def read(str: String): Try[Int]   = Try {str.toInt}
    override def write(t: Int): Option[String] = Some(t.toString)
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
    private val reader = seqReader[ByteRange]
    private val writer = seqWriter[ByteRange]

    override def read(value: String): Try[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           Failure(new IllegalArgumentException("Illegal byte ranges: " + value))

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }
}