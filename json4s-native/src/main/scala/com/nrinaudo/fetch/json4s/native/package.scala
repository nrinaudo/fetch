package com.nrinaudo.fetch.json4s

import java.nio.charset.Charset

import com.nrinaudo.fetch._
import org.json4s._
import org.json4s.native.{JsonMethods, Serialization}

package object native {
  implicit def writer(implicit formats: Formats): EntityWriter[JValue] =
    EntityWriter.text { (j: JValue, out) =>
      Serialization.write(j, out)
      ()
    }.withMediaType(MediaType.Json.charset(Charset.forName("UTF-8")))

  implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))
}
