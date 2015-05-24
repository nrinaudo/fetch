package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

import scala.annotation.implicitNotFound

object EntityWriter {
  def string(m: MediaType): EntityWriter[String] = new TextEntityWriter[String] {
    override def length(str: String, charset: Charset) = Some(str.getBytes(charset).length.toLong)
    override def write(str: String, out: Writer)       = out.write(str)
    override val mediaType                             = m
  }

  def stream(m: MediaType): EntityWriter[InputStream] = new EntityWriter[InputStream] {
    override def length(a: InputStream)                    = None
    override def write(in: InputStream, out: OutputStream) = writeBytes(in, out)
    override val mediaType                                 = m
  }

  def reader(m: MediaType): EntityWriter[Reader] = new TextEntityWriter[Reader] {
    override def length(in: Reader, charset: Charset) = None
    override def write(in: Reader, out: Writer)       = writeChars(in, out)
    override def mediaType                            = m
  }

  def file(m: MediaType): EntityWriter[File] = new EntityWriter[File] {
    override def write(file: File, out: OutputStream) = {
      val in = new BufferedInputStream(new FileInputStream(file))
      try {writeBytes(in, out)}
      finally {in.close()}
    }

    override def length(file: File) = Some(file.length())
    override def mediaType          = m
  }
}

// TODO: I'm not entirely sure why this needs to be contravariant, investigate
@implicitNotFound(msg = "Cannot find an EntityWriter typeclass for ${A}")
trait EntityWriter[-A] {
  def write(a: A, out: OutputStream): Unit
  def length(a: A): Option[Long]
  def mediaType: MediaType
}

/** Specialised version of {{{EntityWriter}}} for text entities.
  *
  * It's important to realise that the charset information is taken from the {{{#mediaType}}} value. If none is set,
  * then {{{#defaultCharset}}} will be used when writing the entity.
  */
trait TextEntityWriter[A] extends EntityWriter[A] {
  // - Charset management ----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns the charset to use when writing this entity.
    *
    * If none was set, returns {{{#defaultCharset}}}.
    */
  def charset: Charset = mediaType.charset.getOrElse(defaultCharset)

  /** Returns the charset to use when none was set.
    *
    * Returns `UTF-8` if not overridden.
    */
  def defaultCharset: Charset = DefaultCharset


  // - EntityWriter implementation -------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override final def write(a: A, out: OutputStream) = {
    val writer = new OutputStreamWriter(out, charset)
    write(a, writer)
    writer.flush()
  }

  override final def length(a: A) = length(a, charset)



  // - Abstract methods ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def write(a: A, out: Writer): Unit
  def length(a: A, charset: Charset): Option[Long]
}