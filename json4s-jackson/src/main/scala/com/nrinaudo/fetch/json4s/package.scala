package com.nrinaudo.fetch

import java.io.OutputStream

import com.fasterxml.jackson.core.JsonGenerator
import org.json4s._
import org.json4s.jackson.JsonMethods

import scala.language.implicitConversions

package object json4s {
  /** Polite mapper that does not close streams it does not own. */
  lazy val mapper = {
    val m = JsonMethods.mapper.copy()
    m.getFactory.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
    m
  }

  implicit def writer(implicit formats: Formats): EntityWriter[JValue] = new EntityWriter[JValue] {
    override def mediaType: MediaType = MediaType.Json.charset(DefaultCharset)
    override def length(a: JValue): Option[Long] = None
    override def write(a: JValue, out: OutputStream): Unit = mapper.writeValue(out, Extraction.decompose(a))
  }

  implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))
}
