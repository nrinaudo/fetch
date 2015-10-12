package com.nrinaudo.fetch.json4s

import java.nio.charset.Charset

import com.nrinaudo.fetch._
import org.json4s._
import org.json4s.native.{JsonMethods, Serialization}

package object native {
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
