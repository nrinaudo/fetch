package fetch.json4s

import fetch._
import org.json4s._
import org.json4s.native.{JsonMethods, Serialization}

package object native {
  implicit def writer(implicit formats: Formats): EntityWriter[JValue] =
    EntityWriter.text { (j: JValue, out) =>
      Serialization.write(j, out)
      ()
    }.withMediaType(MediaType.Json.charset(DefaultCharset))

  implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))(DefaultCharset)
}
