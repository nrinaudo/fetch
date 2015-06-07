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
  implicit val stringHeader: ValueFormat[String] = new ValueFormat[String] {
    override def write(value: String): Option[String] =
    if(value.isEmpty) None
    else Some(value)

    override def read(value: String): Option[String] = Some(value)
  }

  implicit val intHeader: ValueFormat[Int] = ValueFormat.intParam
  implicit val floatHeader: ValueFormat[Float] = ValueFormat.floatParam
  implicit val charsetHeader = ValueFormat.charsetParam




  // - Header specific default formats ---------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Formats dates to the proper RFC compliant syntax. */
  implicit object dateHeader extends ValueFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Option[Date] = HttpDateFormat.synchronized {Try(HttpDateFormat.parse(str)).toOption}
    override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
  }

  implicit object languageHeader extends ValueFormat[Language] {
    override def read (value: String): Option[Language] = Language.parse(value)
    override def write(value: Language): Option[String] = Some(grammar.language(value.main, value.sub))
  }

  implicit object encodingHeader extends ValueFormat[Encoding] {
    override def read(value: String): Option[Encoding] = Encoding.DefaultEncodings.get(value)
    override def write(value: Encoding): Option[String] = Some(value.name)
  }

  implicit object mediaTypeHeader extends ValueFormat[MediaType] {
    override def read(str: String): Option[MediaType] = MediaType.parse(str)
    override def write(t: MediaType): Option[String]  = Some(t.toString)
  }

  implicit object methodHeader extends ValueFormat[Method] {
    override def read(value: String): Option[Method] = Method.parse(value)

    override def write(value: Method): Option[String] = Some(value.name)
  }

  implicit object etagHeader extends ValueFormat[ETag] {
    override def read(value: String): Option[ETag]  = ETag.parse(value)
    override def write(value: ETag): Option[String] = Some(value.toString)
  }

  implicit object byteRangeHeader extends ValueFormat[ByteRange] {
    override def read(value: String): Option[ByteRange]  = ByteRange.parse(value)
    override def write(value: ByteRange): Option[String] = Some(value.toString)
  }

  implicit object byteRangesHeader extends ValueFormat[Seq[ByteRange]] {
    private val reader = compositeReader[ByteRange]
    private val writer = compositeWriter[ByteRange]

    override def read(value: String): Option[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           None

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }

  def empty: Headers = new Headers(Map.empty)
}

case class Headers(override val values: Map[String, String]) extends Parameters[Headers] {
  override def build(values: Map[String, String]): Headers = copy(values)
}