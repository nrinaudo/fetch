package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

import simulacrum.typeclass

import scala.annotation.implicitNotFound

object EntityWriter {
  def binary[A](f: (A, OutputStream) => Unit): EntityWriter[A] = new EntityWriter[A] {
    override def length(a: A) = None
    override def write(a: A, out: OutputStream): Unit = f(a, out)
    override def mediaType: MediaType = MediaType.OctetStream
  }

  def text[A](f: (A, Writer) => Unit): EntityWriter[A] = new EntityWriter[A] {
    override def length(a: A) = None
    override def write(a: A, out: OutputStream) = {
      val writer = new OutputStreamWriter(out, charset)
      f(a, writer)
      writer.flush()
    }
    override def mediaType = MediaType.PlainText
  }

  implicit val chars: EntityWriter[Reader] = text((in, out) => writeChars(in, out))

  implicit val string: EntityWriter[String] = text((s, o) => o.write(s))

  implicit val bytes: EntityWriter[InputStream] = binary((in, out) => writeBytes(in, out))

  implicit val file: EntityWriter[File] =
    bytes.contramap((f: File) => new BufferedInputStream(new FileInputStream(f))).withLength(f => Some(f.length))
}

@implicitNotFound(msg = "Cannot find an EntityWriter instance for ${A}")
@typeclass trait EntityWriter[A] { self =>
  def write(a: A, out: OutputStream): Unit
  def length(a: A): Option[Long]
  def mediaType: MediaType
  def defaultCharset: Charset = DefaultCharset
  def charset: Charset = mediaType.charset.getOrElse(defaultCharset)

  def withDefaultCharset(charset: Charset): EntityWriter[A] = new EntityWriter[A] {
    override def write(a: A, out: OutputStream) = self.write(a, out)
    override def mediaType = self.mediaType
    override def length(a: A) = self.length(a)
    override def defaultCharset: Charset = charset
  }

  def contramap[B](f: B => A): EntityWriter[B] = new EntityWriter[B] {
    override def length(b: B) = self.length(f(b))
    override def mediaType = self.mediaType
    override def write(b: B, out: OutputStream) = self.write(f(b), out)
  }

  def withMediaType(m: MediaType): EntityWriter[A] = new EntityWriter[A] {
    override def mediaType: MediaType = m
    override def length(a: A) = self.length(a)
    override def write(a: A, out: OutputStream) = self.write(a, out)
  }

  def withLength(f: A => Option[Long]): EntityWriter[A] = new EntityWriter[A] {
    override def write(a: A, out: OutputStream) = self.write(a, out)
    override def mediaType = self.mediaType
    override def length(a: A) = f(a)
  }
}
