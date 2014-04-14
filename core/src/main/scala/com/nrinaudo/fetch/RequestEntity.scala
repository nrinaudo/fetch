package com.nrinaudo.fetch

import java.io._
import scala.Some
import java.util.zip.DeflaterOutputStream


// - Raw entities ------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
trait RequestEntity {
  /** Exact length, in bytes, of the entity. */
  def length: Option[Int]

  /** MIME type of the entity. */
  def mimeType: MimeType

  /** Writes this entity to the specified output stream.
    * Implementations should not close the stream.
    */
  def write(out: OutputStream): Unit
}

class StreamEntity(val in: InputStream, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  override def length: Option[Int] = None

  override def write(out: OutputStream) = writeBytes(in, out)
}

class FileEntity(val file: File, override val mimeType: MimeType = MimeType.ApplicationOctetSteam) extends RequestEntity {
  override def length: Option[Int] = Some(file.length().toInt)

  override def write(out: OutputStream) = {
    val in = new BufferedInputStream(new FileInputStream(file))
    try {writeBytes(in, out)}
    finally {in.close()}
  }
}

class DeflatedEntity(val underlying: RequestEntity, f: OutputStream => DeflaterOutputStream) extends RequestEntity {
  override def write(out: OutputStream) {
    val deflate = f(out)
    underlying.write(deflate)
    deflate.finish()
  }

  override def mimeType: MimeType = underlying.mimeType

  override def length: Option[Int] = None
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
    * Implementations should not close the writer.
    */
  override def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }

  def write(writer: Writer): Unit
}

class StringEntity(body: String, mimeType: MimeType = MimeType.TextPlain) extends TextEntity(mimeType) {
  // TODO: this can probably be optimized.
  override lazy val length: Option[Int] = Some(body.getBytes(charset).length)

  override def write(writer: Writer) {
    writer.write(body)
  }
}

class ReaderEntity(in: Reader, mimeType: MimeType = MimeType.TextPlain) extends TextEntity(mimeType) {
  override def length: Option[Int] = None

  override def write(out: Writer) {
    try {writeChars(in, out)}
    finally {in.close()}
  }
}