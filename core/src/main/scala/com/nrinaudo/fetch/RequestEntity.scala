package com.nrinaudo.fetch

import java.io._
import scala.Some

// - Raw entities ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/** Represents the body of an HTTP request.
  * @author Nicolas Rinaudo
  */
trait RequestEntity {
  /** Exact length, in bytes, of the entity.
    * Unknown lengths result in a [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6 chunked]]
    * transfer-coding being used.
    */
  def length: Option[Int]

  /** MIME type of the entity.
    *
    * Note that this will always override whatever value was set for a request's `Content-Type` header.
    */
  def mimeType: MimeType

  /** Writes this entity to the specified output stream.
    *
    * Implementations should not close the specified stream.
    */
  def write(out: OutputStream): Unit
}

/** [[com.nrinaudo.fetch.RequestEntity]] wrapping an `InputStream`.
  *
  * Closing the specified stream, or wrapping it into a buffered implementation if applicable, is left to the
  * responsibility of the caller.
  *
  * @param in       input stream whose content will be written when the entity is submitted.
  * @param mimeType MIME type of the entity.
  */
class StreamEntity(val in: InputStream, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  /** Returns [[None]]. */
  override def length: Option[Int] = None

  override def write(out: OutputStream) = writeBytes(in, out)
}

/** [[RequestEntity]] wrapping a `File`.
  *
  * Note that the specified file will be treated as binary - no charset decoding will occur, and the file's content
  * will be sent as-is.
  *
  * @param file  file whose content will be submitted.
  */
class FileEntity(val file: File, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  /** Returns the length of the file. */
  override def length: Option[Int] = Some(file.length().toInt)

  override def write(out: OutputStream) = {
    val in = new BufferedInputStream(new FileInputStream(file))
    try {writeBytes(in, out)}
    finally {in.close()}
  }
}


// - Text entities -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/**
 * Represents a text-based request entity.
 */
abstract class TextEntity(mime: MimeType) extends RequestEntity {
  override val mimeType = {
    if(mime.charset.isDefined) mime
    else                       mime.charset(DefaultCharset)
  }

  // No need to check for None, this is done at constructor time.
  def charset = mimeType.charset.get

  /** Writes this entity to the specified writer.
    * Implementations should not close the writer.
    */
  override def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }

  def write(writer: Writer): Unit
}

class StringEntity(val body: String, mime: MimeType = MimeType.TextPlain) extends TextEntity(mime) {
  // TODO: this can probably be optimized.
  override lazy val length: Option[Int] = Some(body.getBytes(charset).length)

  override def write(writer: Writer) {
    writer.write(body)
  }
}

class ReaderEntity(in: Reader, mime: MimeType = MimeType.TextPlain) extends TextEntity(mime) {
  override def length: Option[Int] = None

  override def write(out: Writer) {
    try {writeChars(in, out)}
    finally {in.close()}
  }
}