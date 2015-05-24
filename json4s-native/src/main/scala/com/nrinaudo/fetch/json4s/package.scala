package com.nrinaudo.fetch

import java.io.OutputStream

import org.json4s._
import org.json4s.native.JsonMethods

import scala.language.implicitConversions

package object json4s {
  implicit def writer(implicit formats: Formats): EntityWriter[JValue] = new EntityWriter[JValue] {
    override def mediaType: MediaType = MediaType.Json.charset(DefaultCharset)
    override def length(a: JValue): Option[Long] = None
    override def write(a: JValue, out: OutputStream): Unit = write(Extraction.decompose(a), out)
  }

  implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))
}
