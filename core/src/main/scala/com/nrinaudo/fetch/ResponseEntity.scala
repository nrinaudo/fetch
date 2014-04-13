package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.io.{StringWriter, InputStreamReader, InputStream}

/**
 * Represents a raw response entity.
 * @param mime   MIME type of the entity.
 * @param stream stream from which to read the content of the entity.
 */
class ResponseEntity(val mime: Option[MimeType], val stream: InputStream) {
  /** Charset in which the entity is written, if any. */
  def charset: Option[Charset] = for {
    m       <- mime
    charset <- m.charset
  } yield charset

  /** Reads the content of the response and returns it as a string.
    * Note that the underlying stream will be closed after this.
    */
  def text(): String = {
    val writer = new StringWriter()
    try {writeAll(new InputStreamReader(stream, charset getOrElse DefaultCharset), writer)}
    finally {stream.close()}
    writer.toString
  }
}
