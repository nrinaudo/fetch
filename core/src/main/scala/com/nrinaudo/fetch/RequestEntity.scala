package com.nrinaudo.fetch

import java.io._
import scala.Some



// - Raw entities ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait RequestEntity {
  def length: Option[Int]
  def mimeType: MimeType

  /** Writes this entity to the specified output stream.
    * Implementations should close the stream when done.
    */
  def write(out: OutputStream): Unit
}

class StreamEntity(val in: InputStream, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  override def length: Option[Int] = None

  override def write(out: OutputStream) = writeAll(in, out)
}

class FileEntity(val file: File, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  override def length: Option[Int] = Some(file.length().toInt)

  override def write(out: OutputStream) = writeAll(new BufferedInputStream(new FileInputStream(file)), out)
}


// - Text entities -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
/**
 * Represents a text-based request entity.
 * @param mime entity's MIME type. If it does not contain a charset parameter, [[DefaultCharset]]
 *             will be used.
 */
abstract class TextEntity(mime: MimeType) extends RequestEntity {
  override val mimeType = if(mime.charset.isDefined) mime else mime.charset(DefaultCharset)

  // No need to check for None, this is done at constructor time.
  def charset = mimeType.charset.get

  /** Writes this entity to the specified writer.
    * Implementations should close the writer when done.
    */
  override def write(out: OutputStream) = write(new OutputStreamWriter(out, charset))

  def write(writer: Writer): Unit
}

class StringEntity(body: String, mimeType: MimeType = MimeType.TextPlain) extends TextEntity(mimeType) {
  // TODO: this can probably be optimized.
  override lazy val length: Option[Int] = Some(body.getBytes(charset).length)

  override def write(writer: Writer) {
    writer.write(body)
    writer.close()
  }
}

class ReaderEntity(in: Reader, mimeType: MimeType = MimeType.TextPlain) extends TextEntity(mimeType) {
  override def length: Option[Int] = None

  override def write(out: Writer) = writeAll(in, out)
}