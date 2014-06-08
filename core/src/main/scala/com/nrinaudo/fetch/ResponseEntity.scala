package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.io.{FilterInputStream, Reader, InputStreamReader, InputStream}
import com.nrinaudo.fetch.ResponseEntity.EntityParser

object ResponseEntity {
  type EntityParser[T] = ResponseEntity => T
}

/**
 * Represents a raw response entity.
 * @param mediaType media type of the entity.
 * @param stream    stream from which to read the content of the entity.
 */
class ResponseEntity(val mediaType: Option[MediaType], stream: InputStream) {
  require(stream != null, "Response stream should never be null")

  // It appears that some stream implementation (decompressing ones, for instance, such as GZipInputStream) stop reading
  // before read returns -1 - which seems to prevent keeping connections alive.
  // The following is a nasty workaround, but it works.
  private val content = new FilterInputStream(stream) {
    override def close(): Unit = {
      in.read()
      super.close()
    }
  }


  def decode(encoding: Encoding): ResponseEntity = new ResponseEntity(mediaType, encoding.decode(content))

  def as[T: EntityParser]: T = implicitly[EntityParser[T]].apply(this)

  /** Charset in which the entity is written, if any. */
  def charset: Option[Charset] = for {
    m       <- mediaType
    charset <- m.charset
  } yield charset

  /** Executes the specified function on this response entity.
    *
    * This method will take care of closing the underlying stream.
    */
  def withStream[T](f: InputStream => T) =
    try {f(content)}
    finally {content.close()}

  /** Executes the specified function on this response entity.
    *
    * This method will take care of closing the underlying stream.
    */
  def withReader[T](f: Reader => T) = {
    val reader = new InputStreamReader(content, charset getOrElse DefaultCharset)
    try {f(reader)}
    finally {reader.close()}
  }
}
