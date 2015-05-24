package com.nrinaudo.fetch

import java.nio.charset.Charset

import org.json4s._
import org.json4s.native.{JsonMethods, Serialization}

import scala.language.implicitConversions

package object json4s {
  implicit def writer(implicit formats: Formats): EntityWriter[JValue] = new TextEntityWriter[JValue] {
    override def mediaType                             = MediaType.Json.charset(DefaultCharset)
    override def length(a: JValue, charset: Charset)   = None
    override def write(a: JValue, out: java.io.Writer) = {
      Serialization.write(a, out)
      ()
    }
  }

  implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))
}
