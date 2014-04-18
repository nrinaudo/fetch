package com.nrinaudo.fetch

import org.json4s._
import com.nrinaudo.fetch.ResponseEntity.EntityParser
import scala.language.implicitConversions
import org.json4s.jackson.JsonMethods
import com.fasterxml.jackson.core.JsonGenerator

/**
 * @author Nicolas Rinaudo
 */
package object json4s {
  /** Polite mapper that does not close streams it does not own. */
  lazy val mapper = {
    val m = JsonMethods.mapper.copy()
    m.getFactory.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
    m
  }

  implicit def jsonToEntity(json: JValue)(implicit formats: Formats) = RequestEntity.chars {out =>
    mapper.writeValue(out, Extraction.decompose(json)(formats))
  }.mimeType(MimeType.Json.charset(DefaultCharset))

  implicit val Parser: EntityParser[JValue] = (entity: ResponseEntity) => {
    val reader = entity.reader
    try {org.json4s.jackson.JsonMethods.parse(ReaderInput(reader))}
    finally {reader.close()}
  }
}
