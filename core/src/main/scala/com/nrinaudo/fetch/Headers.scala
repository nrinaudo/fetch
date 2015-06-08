package com.nrinaudo.fetch

import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}

import scala.util.Try

object Headers {
  // - Composite header formats ----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def compositeFormat[T: ValueReader: ValueWriter]: ValueFormat[Seq[T]] =
    ValueFormat(compositeReader[T], compositeWriter[T])

  implicit def compositeWriter[T: ValueWriter]: ValueWriter[Seq[T]] =
    ValueWriter((values: Seq[T]) => ValueWriter.sequence(values) map {_.mkString(",")})

  implicit def compositeReader[T: ValueReader]: ValueReader[Seq[T]] =
    ValueReader((s: String) => ValueReader.sequence[T](s.split(',')))



  // - Header specific default formats ---------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // TODO: value class and generic?
  /** Formats dates to the proper RFC compliant syntax. */
  implicit object HttpDate extends ValueFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def read(str: String): Option[Date] = HttpDateFormat.synchronized {Try(HttpDateFormat.parse(str)).toOption}
    override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
  }

  implicit object byteRangesHeader extends ValueFormat[Seq[ByteRange]] {
    private val reader = compositeReader[ByteRange]
    private val writer = compositeWriter[ByteRange]

    override def read(value: String): Option[Seq[ByteRange]] =
      if(value.startsWith("bytes=")) reader.read(value.substring(6))
      else                           None

    override def write(value: Seq[ByteRange]): Option[String] = writer.write(value) map {"bytes=" + _ }
  }
}