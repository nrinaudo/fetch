package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

/** Represents the body of an HTTP request.
  *
  * See [[RequestEntity!]] for various creation helpers.
  *
  * @author Nicolas Rinaudo
  */
trait RequestEntity {
  // - Entity length ---------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Exact length, in bytes, of the entity.
    *
    * Unspecified lengths result in a [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6 chunked]]
    * transfer encoding being used.
    *
    * Note that an entity's length is different from its content length: the former is the entity's actual size while
    * the later is its size after it's been encoded. Any encoder other than [[Encoding.Identity]] will ignore the
    * entity's length and use chunked transfer encoding.
    */
  def length: Option[Int]

  /** Sets the length, in bytes, of the request entity. */
  def length(value: Option[Int]): RequestEntity = new EntityAdapter(this) {
    override val length = value
  }

  def contentLength: Option[Int] =
      if(encoding == Encoding.Identity) length
      else                              None



  // - MIME type -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** MIME type of the entity.
    *
    * Note that this will always override whatever value was set for a request's `Content-Type` header.
    */
  def mimeType: MimeType

  /** Sets the entity's MIME type. */
  def mimeType(value: MimeType): RequestEntity = new EntityAdapter(this) {
    override val mimeType = value
  }


  // - Content Encoding ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Content-encoding used when submitting the entity.
    *
    * The defaults is [[Encoding.Identity]], and should only be changed when certain that the remote host will accepts
    * encoded entities.
    *
    * This value will always override the request's `Content-Encoding` HTTP header at submission time.
    */
  def encoding: Encoding = Encoding.Identity

  /** Uses GZIP content-encoding when submitting the entity. */
  def gzip: RequestEntity = encoding(Encoding.Gzip)

  /** Uses Deflate content-encoding when submitting the entity. */
  def deflate: RequestEntity = encoding(Encoding.Deflate)

  /** Uses the specified encoding when submitting the entity.
    *
    * Note that not all servers support submission entity encoding. Only use this if certain that the server won't
    * reject it.
    *
    * Common encodings have helper functions: [[gzip]] and [[deflate]].
    */
  def encoding(value: Encoding): RequestEntity = new EntityAdapter(this) {
    override val encoding: Encoding = value
  }


  // - Submission ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Writes this entity to the specified output stream.
    *
    * Note that this method should not be called directly, as it ignores modifiers such that content encoding.
    * When you absolutely need to write the entity (such as when, say, implementing a non-default
    * [[com.nrinaudo.fetch.Request.HttpEngine HttpEngine]]), you should call [[RequestEntity!.apply]] instead.
    *
    * Implementations should not close the specified stream.
    */
  def write(out: OutputStream): Unit

  def apply(out: OutputStream) {
    val stream = encoding.encode(out)
    try {write(stream)}
    finally {stream.close()}
  }
}

object RequestEntity {
  def bytes(f: OutputStream => Unit): RequestEntity = new RequestEntity {
    override def write(out: OutputStream): Unit = f(out)
    override val mimeType: MimeType = MimeType.ApplicationOctetSteam
    override val length: Option[Int] = None
  }

  def chars(f: Writer => Unit): RequestEntity = new TextEntity {
    override def write(out: Writer): Unit = f(out)
    override def mimeType: MimeType = MimeType.TextPlain.charset(DefaultCharset)
    override val length: Option[Int] = None
  }

  def apply(in: InputStream): RequestEntity = bytes((out: OutputStream) => writeBytes(in, out))

  def apply(in: Reader): RequestEntity = chars((out: Writer) => writeChars(in, out))

  def apply(file: File): RequestEntity = new RequestEntity {
    override def write(out: OutputStream): Unit = {
      val in = new BufferedInputStream(new FileInputStream(file))
      try {writeBytes(in, out)}
      finally {in.close()}
    }

    override val mimeType: MimeType = MimeType.ApplicationOctetSteam

    override lazy val length: Option[Int] = Some(file.length().toInt)
  }

  def apply(str: String): RequestEntity = new TextEntity {
    // TODO: this can probably be optimized.
    override lazy val length: Option[Int] = Some(str.getBytes(mimeType.charset.get).length)

    override val mimeType: MimeType = MimeType.TextPlain.charset(DefaultCharset)

    override def write(writer: Writer) = writer.write(str)
  }
}

trait TextEntity extends RequestEntity {
  def write(out: Writer): Unit

  def charset: Charset = mimeType.charset.getOrElse(DefaultCharset)

  override def write(out: OutputStream) {
    val writer = new OutputStreamWriter(out, charset)
    write(writer)
    writer.flush()
  }
}

class EntityAdapter(val underlying: RequestEntity) extends RequestEntity {
  override def write(out: OutputStream): Unit = underlying.write(out)
  override def mimeType: MimeType = underlying.mimeType
  override def length: Option[Int] = underlying.length
  override def encoding: Encoding = underlying.encoding
}
