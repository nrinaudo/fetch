package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import scala.util.{Failure, Success, Try}
import java.nio.charset.Charset

object Headers {
  // - Composite header formats ----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def compositeFormat[T: ValueReader: ValueWriter]: ValueFormat[Seq[T]] =
    ValueFormat(compositeReader[T], compositeWriter[T])

  implicit def compositeWriter[T: ValueWriter]: ValueWriter[Seq[T]] =
    ValueWriter((values: Seq[T]) => ValueWriter.sequence(values) map {_.mkString(",")})

  implicit def compositeReader[T: ValueReader]: ValueReader[Seq[T]] =
    ValueReader((s: String) => ValueReader.sequence[T](s.split(',')))


  // - Generic default formats -----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val StringFormat: ValueFormat[String] = new ValueFormat[String] {
    override def write(value: String): Option[String] =
    if(value.isEmpty) None
    else Some(value)

    override def read(value: String): Try[String] = Success(value)
  }

  implicit val IntFormat: ValueFormat[Int] = ValueFormat.Ints


  // - Header specific default formats ---------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Formats dates to the proper RFC compliant syntax. */
  implicit object DateFormat extends ValueFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Try[Date]      = HttpDateFormat.synchronized {Try {HttpDateFormat.parse(str)}}
    override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
  }

  private val LanguagePattern = """([^-]+)(?:-([^-]+))?""".r
  implicit object LanguageFormat extends ValueFormat[Locale] {
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

  implicit object CharsetFormat extends ValueFormat[Charset] {
    override def read(value: String): Try[Charset]     = Try {Charset.forName(value)}
    override def write(value: Charset): Option[String] = Some(value.name())
  }

  implicit object EncodingFormat extends ValueFormat[Encoding] {
    override def read(value: String): Try[Encoding] =
      Encoding.DefaultEncodings.get(value) map {Success(_)} getOrElse Failure(new IllegalArgumentException("Unsupported encoding: " + value))
    override def write(value: Encoding): Option[String] = Some(value.name)
  }

  implicit object MimeTypeFormat extends ValueFormat[MimeType] {
    override def read(str: String): Try[MimeType]   = Try {MimeType(str)}
    override def write(t: MimeType): Option[String] = Some(t.toString)
  }

  implicit object MethodFormat extends ValueFormat[Method] {
    override def read(value: String): Try[Method] = Method.unapply(value) map {Success(_)} getOrElse {
      throw new IllegalArgumentException("Unsupported method: " + value)
    }

    override def write(value: Method): Option[String] = Some(value.name)
  }

  implicit object ETagFormat extends ValueFormat[ETag] {
    override def read(value: String): Try[ETag]     = Try {ETag(value)}
    override def write(value: ETag): Option[String] = Some(value.toString)
  }

  implicit object ByteRangeFormat extends ValueFormat[ByteRange] {
    override def read(value: String): Try[ByteRange]     = Try {ByteRange(value)}
    override def write(value: ByteRange): Option[String] = Some(value.toString)
  }

  implicit object ByteRangesFormat extends ValueFormat[Seq[ByteRange]] {
    private val reader = compositeReader[ByteRange]
    private val writer = compositeWriter[ByteRange]

    override def read(value: String): Try[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           Failure(new IllegalArgumentException("Illegal byte ranges: " + value))

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }
}

class Headers(val values: Map[String, String] = Map()) {
  def apply[T: ValueReader](name: String): T = implicitly[ValueReader[T]].read(values(name)).get

  def getOpt[T: ValueReader](name: String): Option[T] = for {
    raw    <- values.get(name)
    parsed <- implicitly[ValueReader[T]].read(raw).toOption
  } yield parsed

  def get[T: ValueReader](name: String): Option[Try[T]] = values.get(name) map implicitly[ValueReader[T]].read

  def set[T: ValueWriter](name: String, value: T): Headers =
    implicitly[ValueWriter[T]].write(value).fold(this)(set(name, _))

  def set(name: String, value: String): Headers = new Headers(values + (name -> value))

  def setIfEmpty[T: ValueWriter](name: String, value: T): Headers =
    if(contains(name)) this
    else               set(name, value)

  def remove(name: String): Headers =
    if(contains(name)) new Headers(values - name)
    else               this

  def contains(name: String): Boolean = values.contains(name)
}