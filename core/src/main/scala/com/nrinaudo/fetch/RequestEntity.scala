package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

trait RequestEntityLike[+Self <: RequestEntityLike[Self]] {
  this: Self =>

  /** Length, in bytes, of the request entity.
    *
    * Note that this is different from the [[contentLength content length]], which represents the number of bytes that
    * will actually be transferred. This can be less than the entity's length if, for example, the [[gzip]] encoding
    * is used.
    */
  def length: Option[Long]

  /** Media type of the request entity. */
  def mediaType: MediaType

  /** Encoding in which the request entity should be transferred.
    *
    * Some but not all servers will accept encodings other than [[Encoding.Identity]]. When such is known to be the
    * case, using [[Encoding.Gzip]], for example, can yield significant performance gains.
    */
  def encoding: Encoding

  /** Creates a new instance of this class with the specified values. */
  protected def build(mediaType: MediaType, encoding: Encoding): Self

  /** Writes this request entity to the specified output stream. */
  protected def write(out: OutputStream): Unit

  /** Number of bytes that will be transferred when this request entity is sent to a remote host. */
  def contentLength: Option[Long] =
    if(encoding == Encoding.Identity) length
    else                              None

  /** Sets this request entity's media type. */
  def mediaType(value: MediaType): Self = build(value, encoding)

  /** Sets this request entity's transfer encoding to [[Encoding.Gzip]]. */
  def gzip: Self = encoding(Encoding.Gzip)

  /** Sets this request entity's transfer encoding to [[Encoding.Deflate]]. */
  def deflate: Self = encoding(Encoding.Deflate)

  /** Sets this request entity's transfer encoding to the specified value. */
  def encoding(value: Encoding): Self = build(mediaType, value)

  /** Writes this request entity to the specified output stream, applying its transfer encoding if applicable. */
  def apply(out: OutputStream) {
    val stream = encoding.encode(out)
    try {write(stream)}
    finally {stream.close()}
  }
}

object RequestEntity {
  // - Stream helpers --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class StreamRequestEntity(private val f: OutputStream => Unit,
                                    override val mediaType: MediaType = MediaType.OctetStream,
                                    override val encoding: Encoding = Encoding.Identity) extends RequestEntity {
    override def length: Option[Long] = None

    override protected def build(mediaType: MediaType, encoding: Encoding): StreamRequestEntity =
      new StreamRequestEntity(f, mediaType, encoding)

    override protected def write(out: OutputStream): Unit = f(out)
  }

  def bytes(f: OutputStream => Unit): RequestEntity = new StreamRequestEntity(f)

  def apply(in: InputStream): RequestEntity = bytes(writeBytes(in, _))


  // - Writer helpers --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class WriterRequestEntity(private val f: Writer => Unit,
                                    override val mediaType: MediaType = MediaType.TextPlain,
                                    override val encoding: Encoding = Encoding.Identity) extends TextRequestEntity {
    override def length: Option[Long] = None

    override protected def build(mediaType: MediaType, encoding: Encoding): WriterRequestEntity =
      new WriterRequestEntity(f, mediaType, encoding)

    override protected def write(out: Writer): Unit = f(out)
  }

  def chars(f: Writer => Unit): TextRequestEntity = new WriterRequestEntity(f)

  def apply(in: Reader): TextRequestEntity = chars(writeChars(in, _))



  // - String helper ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class StringEntity(val content: String, override val mediaType: MediaType, override val encoding: Encoding)
    extends TextRequestEntity with RequestEntityLike[StringEntity] {
    override lazy val length: Option[Long] = Some(content.getBytes(charset).length)
    override protected def write(out: Writer): Unit = out.write(content)
    override protected def build(mediaType: MediaType, encoding: Encoding): StringEntity = new StringEntity(content, mediaType, encoding)
    override def toString = "String(%s)" format content
  }

  def apply(str: String): TextRequestEntity =
    new StringEntity(str, MediaType.TextPlain, Encoding.Identity)



  // - File helper -----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private class FileEntity(val file: File, override val mediaType: MediaType, override val encoding: Encoding)
    extends RequestEntity with RequestEntityLike[FileEntity] {
    override def write(out: OutputStream) {
      val in = new BufferedInputStream(new FileInputStream(file))
      try {writeBytes(in, out)}
      finally {in.close()}
    }
    override lazy val length: Option[Long] = Some(file.length())
    override protected def build(mediaType: MediaType, encoding: Encoding): FileEntity =
      new FileEntity(file, mediaType, encoding)
    override def toString = "File(%s)" format file
  }

  def apply(file: File): RequestEntity = new FileEntity(file, MediaType.OctetStream, Encoding.Identity)
}

trait RequestEntity extends RequestEntityLike[RequestEntity]

trait TextRequestEntity extends RequestEntity with RequestEntityLike[TextRequestEntity] {
  def charset: Charset = mediaType.charset.getOrElse(DefaultCharset)

  protected def write(out: Writer): Unit

  override protected def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }
}