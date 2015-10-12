package com.nrinaudo.fetch.json4s

import java.nio.charset.Charset

import com.fasterxml.jackson.core.JsonGenerator
import com.nrinaudo.fetch._
import org.json4s._
import org.json4s.jackson.JsonMethods

package object jackson {
  /** Polite mapper that does not close streams it does not own. */
    lazy val mapper = {
      val m = JsonMethods.mapper.copy()
      m.getFactory.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
      m
    }

    implicit def writer(implicit formats: Formats): EntityWriter[JValue] = new TextEntityWriter[JValue] {
      override def mediaType                             = MediaType.Json.charset(DefaultCharset)
      override def write(a: JValue, out: java.io.Writer) = mapper.writeValue(out, Extraction.decompose(a))
      override def length(a: JValue, charset: Charset)   = None
    }

    implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))
}
