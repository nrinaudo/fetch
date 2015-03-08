package com.nrinaudo.fetch

import org.json4s._
import com.nrinaudo.fetch.ResponseEntity.EntityParser
import org.json4s.native.Serialization._
import scala.language.implicitConversions
import org.json4s.native.JsonMethods

package object json4s {
  implicit def jsonToEntity(json: JValue)(implicit formats: Formats) = RequestEntity.chars {out => write(json, out); ()}
      .mediaType(MediaType.Json.charset(DefaultCharset))

  implicit val Parser: EntityParser[JValue] = (entity: ResponseEntity) =>
    entity.withReader {in => JsonMethods.parse(ReaderInput(in))}
}
