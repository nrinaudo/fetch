package com.nrinaudo.fetch

import java.util.{TimeZone, Locale, Date}
import java.text.SimpleDateFormat
import java.nio.charset.Charset

/** Serializes / deserializes headers of type `T`. */
trait HeaderFormat[T] {
  def format(value: T): String
  def parse(value: String): T
}

object Headers {
  implicit def seqFormat[T: HeaderFormat]: HeaderFormat[Seq[T]] = new HeaderFormat[Seq[T]] {
    override def parse(value: String): Seq[T] = value.split(',').map(implicitly[HeaderFormat[T]].parse)
    override def format(value: Seq[T]): String = value.map(implicitly[HeaderFormat[T]].format).mkString(",")
  }

  /** Formats dates to the proper RFC compliant syntax. */
  implicit val DateFormat = new HeaderFormat[Date] {
    private val HttpDateFormat = {
      val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
      format.setTimeZone(TimeZone.getTimeZone("GMT"))
      format
    }

    override def parse(str: String): Date = HttpDateFormat.synchronized {HttpDateFormat.parse(str)}
    override def format(date: Date): String = HttpDateFormat.synchronized {HttpDateFormat.format(date)}

  }

  private val LanguagePattern = """([^-]+)(?:-([^-]+))?""".r
  implicit val LanguageFormat = new HeaderFormat[Locale] {
    override def parse(value: String): Locale = value match {
      case LanguagePattern(lang, country) =>
        if(country == null) new Locale(lang)
        else                new Locale(lang, country)

        case _ => throw new IllegalArgumentException("Illegal language definition: " + value)
    }

    override def format(value: Locale): String = {
      var v = value.getLanguage
      if(!value.getCountry.isEmpty) v += "-" + value.getCountry
      v
    }
  }

  implicit val CharsetFormat = new HeaderFormat[Charset] {
    override def parse(value: String): Charset = Charset.forName(value)
    override def format(value: Charset): String = value.name()
  }

  implicit val EncodingFormat = new HeaderFormat[Encoding] {
    override def parse(value: String): Encoding = Encoding.DefaultEncodings.get(value).getOrElse {
      throw new IllegalArgumentException("Unsupported encoding: " + value)
    }
    override def format(value: Encoding): String = value.name
  }

  implicit val MimeTypeFormat = new HeaderFormat[MimeType] {
    override def parse(str: String): MimeType = MimeType(str)
    override def format(t: MimeType): String = t.toString
  }

  implicit val StringFormat = new HeaderFormat[String] {
    override def parse(str: String): String = str
    override def format(t: String): String = t
  }

  implicit val ETagFormat = new HeaderFormat[ETag] {
    override def format(value: ETag): String = value.toString
    override def parse(value: String): ETag = ETag(value)
  }

  implicit val ByteRangeFormat = new HeaderFormat[ByteRange] {
    override def parse(value: String): ByteRange = ByteRange(value)

    override def format(value: ByteRange): String = value.toString
  }

  implicit val ByteRangesFormat = new HeaderFormat[Seq[ByteRange]] {
    override def parse(value: String): Seq[ByteRange] =
      if(value.startsWith("bytes=")) value.substring(6).split(',').map(r => ByteRangeFormat.parse(r.trim))
      else                           throw new IllegalArgumentException("Illegal byte range list: " + value)
    override def format(value: Seq[ByteRange]): String = {
      val builder = new StringBuilder("bytes=")
      var first   = true

      value.foreach { range =>
        if(first) first = false
        else      builder.append(',')
        builder.append(ByteRangeFormat.format(range))
      }
      builder.result()
    }
  }
}

/** Represents the headers associated with an HTTP request or response. */
class Headers(val values: Map[String, String] = Map()) {
  // - Generic header setting / getting --------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def get[T: HeaderFormat](name: String): Option[T] = values.get(name).map(implicitly[HeaderFormat[T]].parse)

  def set[T: HeaderFormat](name: String, value: T): Headers = {
    val formatted = implicitly[HeaderFormat[T]].format(value)

    if(formatted.isEmpty) this
    else                  new Headers(values + (name -> formatted))
  }

  def setIfEmpty[T: HeaderFormat](name: String, value: T): Headers =
    if(contains(name)) this
    else               set(name, value)

  def remove(name: String): Headers =
    if(values.contains(name)) new Headers(values - name)
    else                      this

  def contains(name: String): Boolean = values.contains(name)
}