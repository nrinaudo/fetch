package com.nrinaudo.fetch

import java.nio.charset.Charset
import java.io.{OutputStream, Reader, InputStreamReader, InputStream}
import com.nrinaudo.fetch.ResponseEntity.EntityParser

import scala.annotation.implicitNotFound

object ResponseEntity {
  @implicitNotFound(msg = "Cannot find an EntityParser type class for ${T}")
  type EntityParser[T] = ResponseEntity => T
}

/** Represents a raw response entity.
  * @param mediaType media type of the entity.
  * @param content   stream from which to read the content of the entity.
  */
class ResponseEntity(val mediaType: Option[MediaType], val content: InputStream) {
  def decode(encoding: Encoding): ResponseEntity = new ResponseEntity(mediaType, encoding.decode(content))

  def as[T: EntityParser]: T = implicitly[EntityParser[T]].apply(this)

  /** Charset in which the entity is written, if any. */
  def charset: Option[Charset] = for {
    m       <- mediaType
    charset <- m.charset
  } yield charset

  /** Reads the whole response and discards it.
    *
    * This method is useful when callers have no interest in the response's content, but want to re-use connections if
    * possible. If keep-alive is not desirable / enabled, the [[ignore]] method might be preferred.
    */
  def empty(): Unit = withStream { s =>
    writeBytes(s, new OutputStream {
      override def write(b: Int): Unit = {}
    })
  }

  /** Ignores this entity.
    *
    * This method will close the underlying stream without reading its content. This is useful when callers have no
    * interest in the response entity and don't want to to through the trouble of reading the response.
    *
    * Do note, however, that calling this method will prevent the underlying connection from being re-used by
    * keep-alive.
    */
  def ignore(): Unit = content.close()

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
