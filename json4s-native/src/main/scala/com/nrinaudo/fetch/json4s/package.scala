package com.nrinaudo.fetch

import org.json4s._
import java.io.Writer
import com.nrinaudo.fetch.ResponseEntity.EntityParser

/**
 * @author Nicolas Rinaudo
 */
package object json4s {
  implicit class JsonEntity(val json: JValue)(implicit val formats: Formats) extends TextEntity(MimeType.Json) {
    override def write(writer: Writer): Unit = org.json4s.native.Serialization.write(json, writer)
    override def length: Option[Int] = None
  }

  implicit val Parser: EntityParser[JValue] = (entity: ResponseEntity) => {
    val reader = entity.reader
    try {org.json4s.native.JsonMethods.parse(ReaderInput(reader))}
    finally {reader.close()}
  }
}
