package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import scala.util.{Failure, Success, Try}

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

    override def read(value: String): Option[String] = Some(value)
  }

  implicit val IntFormat: ValueFormat[Int] = ValueFormat.Ints
  implicit val FloatFormat: ValueFormat[Float] = ValueFormat.Floats
  implicit val CharsetFormat = ValueFormat.Charsets




  // - Header specific default formats ---------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Formats dates to the proper RFC compliant syntax. */
  implicit object DateFormat extends ValueFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Option[Date] = HttpDateFormat.synchronized {Try(HttpDateFormat.parse(str)).toOption}
    override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
  }

  implicit object LanguageFormat extends ValueFormat[Language] {
    override def read (value: String): Option[Language] = Language.parse(value)
    override def write(value: Language): Option[String] = Some(value.toString)
  }

  implicit object EncodingFormat extends ValueFormat[Encoding] {
    override def read(value: String): Option[Encoding] = Encoding.DefaultEncodings.get(value)
    override def write(value: Encoding): Option[String] = Some(value.name)
  }

  implicit object MimeTypeFormat extends ValueFormat[MimeType] {
    override def read(str: String): Option[MimeType] = MimeType.parse(str)
    override def write(t: MimeType): Option[String]  = Some(t.toString)
  }

  implicit object MethodFormat extends ValueFormat[Method] {
    override def read(value: String): Option[Method] = Method.parse(value)

    override def write(value: Method): Option[String] = Some(value.name)
  }

  implicit object ETagFormat extends ValueFormat[ETag] {
    override def read(value: String): Option[ETag]  = ETag.parse(value)
    override def write(value: ETag): Option[String] = Some(value.toString)
  }

  implicit object ByteRangeFormat extends ValueFormat[ByteRange] {
    override def read(value: String): Option[ByteRange]  = ByteRange.parse(value)
    override def write(value: ByteRange): Option[String] = Some(value.toString)
  }

  implicit object ByteRangesFormat extends ValueFormat[Seq[ByteRange]] {
    private val reader = compositeReader[ByteRange]
    private val writer = compositeWriter[ByteRange]

    override def read(value: String): Option[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           None

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }
}

class Headers(val values: Map[String, String] = Map()) extends Parameters[Headers] {
  override def build(values: Map[String, String]): Headers = new Headers(values)
}