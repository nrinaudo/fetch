package com.nrinaudo.fetch

import org.json4s._
import com.nrinaudo.fetch.ResponseEntity.EntityParser
import scala.language.implicitConversions
import org.json4s.jackson.JsonMethods
import com.fasterxml.jackson.core.JsonGenerator

package object json4s {
  /** Polite mapper that does not close streams it does not own. */
  lazy val mapper = {
    val m = JsonMethods.mapper.copy()
    m.getFactory.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
    m
  }

  implicit def jsonToEntity(json: JValue)(implicit formats: Formats) = RequestEntity.chars {out =>
    mapper.writeValue(out, Extraction.decompose(json)(formats))
  }.mediaType(MediaType.Json.charset(DefaultCharset))

  implicit val Parser: EntityParser[JValue] = (entity: ResponseEntity) =>
    entity.withReader {in => JsonMethods.parse(ReaderInput(in))}
}
