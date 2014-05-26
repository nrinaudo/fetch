package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

trait RequestEntityLike[+Self <: RequestEntityLike[Self]] {
  this: Self =>
  def length: Option[Long]
  val mimeType: MimeType
  val encoding: Encoding

  protected def copy(mimeType: MimeType, encoding: Encoding): Self

  def contentLength: Option[Long] =
    if(encoding == Encoding.Identity) length
    else                              None

  def mimeType(value: MimeType): Self = copy(value, encoding)

  def gzip: Self = encoding(Encoding.Gzip)

  def deflate: Self = encoding(Encoding.Deflate)

  def encoding(value: Encoding): Self = copy(mimeType, value)

  def write(out: OutputStream): Unit

  def apply(out: OutputStream) {
    val stream = encoding.encode(out)
    try {write(stream)}
    finally {stream.close()}
  }
}

trait RequestEntity extends RequestEntityLike[RequestEntity]

trait TextRequestEntity extends RequestEntity with RequestEntityLike[TextRequestEntity] {
  def write(out: Writer): Unit

  def charset: Charset = mimeType.charset.getOrElse(DefaultCharset)

  override def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }
}

class StreamRequestEntity(private val f: OutputStream => Unit,
                    override val mimeType: MimeType = MimeType.ApplicationOctetSteam,
                    override val encoding: Encoding = Encoding.Identity) extends RequestEntity {
  override def length: Option[Long] = None

  override protected def copy(mimeType: MimeType, encoding: Encoding): StreamRequestEntity =
    new StreamRequestEntity(f, mimeType, encoding)

  override def write(out: OutputStream): Unit = f(out)
}

class WriterRequestEntity(private val f: Writer => Unit,
                        override val mimeType: MimeType = MimeType.TextPlain.charset(DefaultCharset),
                        override val encoding: Encoding = Encoding.Identity) extends TextRequestEntity {
  override def length: Option[Long] = None

  override protected def copy(mimeType: MimeType, encoding: Encoding): WriterRequestEntity =
    new WriterRequestEntity(f, mimeType, encoding)

  override def write(out: Writer): Unit = f(out)
}

object RequestEntity {
  def bytes(f: OutputStream => Unit): RequestEntity = new StreamRequestEntity(f)

  def chars(f: Writer => Unit): TextRequestEntity = new WriterRequestEntity(f)

  def apply(in: InputStream): RequestEntity = bytes(writeBytes(in, _))

  def apply(in: Reader): TextRequestEntity = chars(writeChars(in, _))

  private def copyFile(file: File, out: OutputStream) {
    val in = new BufferedInputStream(new FileInputStream(file))
    try {writeBytes(in, out)}
    finally {in.close()}
  }

  def apply(file: File): RequestEntity = new StreamRequestEntity(copyFile(file, _)) {
    override lazy val length: Option[Long] = Some(file.length())
  }

  def apply(str: String): TextRequestEntity = new WriterRequestEntity(_.write(str)) {
    override lazy val length: Option[Long] = Some(str.getBytes(charset).length)
  }
}