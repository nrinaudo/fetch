package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

trait RequestEntityLike[+Self <: RequestEntityLike[Self]] extends (OutputStream => Unit) {
  this: Self =>

  /** Length, in bytes, of the request entity.
    *
    * Note that this is different from the [[contentLength content length]], which represents the number of bytes that
    * will actually be transferred. This can be less than the entity's length if, for example, the [[gzip]] encoding
    * is used.
    */
  def length: Option[Long]

  /** MIME type of the request entity. */
  def mimeType: MimeType

  /** Encoding in which the request entity should be transferred.
    *
    * Some but not all servers will accept encodings other than [[Encoding.Identity]]. When such is known to be the
    * case, using [[Encoding.Gzip]], for example, can yield significant performance gains.
    */
  def encoding: Encoding

  /** Creates a new instance of this class with the specified values. */
  protected def build(mimeType: MimeType, encoding: Encoding): Self

  /** Writes this request entity to the specified output stream. */
  protected def write(out: OutputStream): Unit

  /** Number of bytes that will be transferred when this request entity is sent to a remote host. */
  def contentLength: Option[Long] =
    if(encoding == Encoding.Identity) length
    else                              None

  /** Sets this request entity's MIME type. */
  def mimeType(value: MimeType): Self = build(value, encoding)

  /** Sets this request entity's transfer encoding to [[Encoding.Gzip]]. */
  def gzip: Self = encoding(Encoding.Gzip)

  /** Sets this request entity's transfer encoding to [[Encoding.Deflate]]. */
  def deflate: Self = encoding(Encoding.Deflate)

  /** Sets this request entity's transfer encoding to the specified value. */
  def encoding(value: Encoding): Self = build(mimeType, value)

  /** Writes this request entity to the specified output stream, applying its transfer encoding if applicable. */
  override def apply(out: OutputStream) {
    val stream = encoding.encode(out)
    try {write(stream)}
    finally {stream.close()}
  }
}

object RequestEntity {
  def bytes(f: OutputStream => Unit): RequestEntity = new StreamRequestEntity(f)

  def chars(f: Writer => Unit): TextRequestEntity = new WriterRequestEntity(f)

  def apply(in: InputStream): RequestEntity = bytes(writeBytes(in, _))

  def apply(in: Reader): TextRequestEntity = chars(writeChars(in, _))

  def apply(str: String): TextRequestEntity = new WriterRequestEntity(_.write(str)) {
    override lazy val length: Option[Long] = Some(str.getBytes(charset).length)
  }

  private def copyFile(file: File, out: OutputStream) {
    val in = new BufferedInputStream(new FileInputStream(file))
    try {writeBytes(in, out)}
    finally {in.close()}
  }

  def apply(file: File): RequestEntity = new StreamRequestEntity(copyFile(file, _)) {
    override lazy val length: Option[Long] = Some(file.length())
  }

}

trait RequestEntity extends RequestEntityLike[RequestEntity]

trait TextRequestEntity extends RequestEntity with RequestEntityLike[TextRequestEntity] {
  def charset: Charset = mimeType.charset.getOrElse(DefaultCharset)

  protected def write(out: Writer): Unit

  override protected def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }
}

class StreamRequestEntity(private val f: OutputStream => Unit,
                          override val mimeType: MimeType = MimeType.ApplicationOctetSteam,
                          override val encoding: Encoding = Encoding.Identity) extends RequestEntity {
  override def length: Option[Long] = None

  override protected def build(mimeType: MimeType, encoding: Encoding): StreamRequestEntity =
    new StreamRequestEntity(f, mimeType, encoding)

  override protected def write(out: OutputStream): Unit = f(out)
}

class WriterRequestEntity(private val f: Writer => Unit,
                          override val mimeType: MimeType = MimeType.TextPlain.charset(DefaultCharset),
                          override val encoding: Encoding = Encoding.Identity) extends TextRequestEntity {
  override def length: Option[Long] = None

  override protected def build(mimeType: MimeType, encoding: Encoding): WriterRequestEntity =
    new WriterRequestEntity(f, mimeType, encoding)

  override protected def write(out: Writer): Unit = f(out)
}

