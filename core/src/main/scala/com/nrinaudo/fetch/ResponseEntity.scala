package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.io.{Reader, InputStreamReader, InputStream}
import com.nrinaudo.fetch.ResponseEntity.EntityParser

object ResponseEntity {
  type EntityParser[T] = ResponseEntity => T
}

/**
 * Represents a raw response entity.
 * @param mime   MIME type of the entity.
 * @param stream stream from which to read the content of the entity.
 */
class ResponseEntity(val mime: Option[MimeType], private val stream: InputStream) {
  require(stream != null, "Response stream should never be null")

  def decode(encoding: Encoding): ResponseEntity = new ResponseEntity(mime, encoding.decode(stream))

  def as[T : EntityParser] = implicitly[EntityParser[T]].apply(this)

  /** Charset in which the entity is written, if any. */
  def charset: Option[Charset] = for {
    m       <- mime
    charset <- m.charset
  } yield charset

  /** Executes the specified function on this response entity.
    *
    * This method will take care of closing the underlying stream.
    */
  def withStream[T](f: InputStream => T) =
    try {f(stream)}
    finally {stream.close()}

  /** Executes the specified function on this response entity.
    *
    * This method will take care of closing the underlying stream.
    */
  def withReader[T](f: Reader => T) = {
    val reader = new InputStreamReader(stream, charset getOrElse DefaultCharset)
    try {f(reader)}
    finally {reader.close()}
  }
}
