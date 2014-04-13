package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.io.{StringWriter, InputStreamReader, InputStream}

class ResponseEntity(val mime: Option[MimeType], val stream: InputStream) {
  def charset: Option[Charset] = for {
    m       <- mime
    charset <- m.charset
  } yield charset

  def text(): String = {
    val writer = new StringWriter()
    writeAll(new InputStreamReader(stream, charset getOrElse DefaultCharset), writer)
    writer.toString
  }
}
