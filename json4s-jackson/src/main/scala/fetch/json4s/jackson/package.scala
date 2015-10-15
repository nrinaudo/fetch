package fetch.json4s

import com.fasterxml.jackson.core.JsonGenerator
import fetch.{EntityReader, EntityWriter, MediaType, _}
import org.json4s._
import org.json4s.jackson.JsonMethods

package object jackson {
  /** Polite mapper that does not close streams it does not own. */
    lazy val mapper = {
      val m = JsonMethods.mapper.copy()
      m.getFactory.disable(JsonGenerator.Feature.AUTO_CLOSE_TARGET)
      m
    }

    implicit def writer(implicit formats: Formats): EntityWriter[JValue] = EntityWriter.text((j: JValue, out) =>
      mapper.writeValue(out, Extraction.decompose(j))
    ).withMediaType(MediaType.Json.charset(DefaultCharset))

    implicit val reader: EntityReader[JValue] = EntityReader.chars(in => JsonMethods.parse(ReaderInput(in)))(DefaultCharset)
}
