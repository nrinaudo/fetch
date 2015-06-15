package com.nrinaudo.fetch

import java.text.SimpleDateFormat
import java.util.{TimeZone, Locale, Date}

import scala.util.Try

/** HTTP formatter for dates. This is not in the default imports as it's a cumbersome format that is unlikely to be
  * anybody's choice.
  */
object HttpDate extends ValueFormat[Date] {
  private val HttpDateFormat = {
    val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss z", Locale.US)
    format.setTimeZone(TimeZone.getTimeZone("GMT"))
    format
  }

  override def read(str: String): Option[Date] = HttpDateFormat.synchronized {Try(HttpDateFormat.parse(str)).toOption}
  override def write(date: Date): Option[String] = Some(HttpDateFormat.synchronized {HttpDateFormat.format(date)})
}