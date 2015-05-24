package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

object EntityReader {
  def bytes[A](f: InputStream => A): EntityReader[A] = new BinaryEntityReader[A] {
    override def read(input: InputStream): A = f(input)
  }

  def chars[A](f: Reader => A): EntityReader[A] = new TextEntityReader[A] {
    override def read(input: Reader): A = f(input)
  }

  implicit val string: EntityReader[String] = chars { input =>
    val writer = new StringWriter()
    writeChars(input, writer)
    writer.toString
  }

  implicit val int: EntityReader[Int]         = string.map(_.toInt)
  implicit val float: EntityReader[Float]     = string.map(_.toFloat)
  implicit val double: EntityReader[Double]   = string.map(_.toDouble)
  implicit val long: EntityReader[Long]       = string.map(_.toLong)
  implicit val boolean: EntityReader[Boolean] = string.map(_.toBoolean)

  val ignore: EntityReader[Unit] = bytes(_ => ())
  val empty: EntityReader[Unit] = bytes(writeBytes(_, new OutputStream {
    override def write(b: Int): Unit = {}
  }))
}

sealed trait EntityReader[A] { self =>
  def read(input: InputStream, mediaType: Option[MediaType]): A

  def map[B](f: A => B): EntityReader[B] = new EntityReader[B] {
    override def read(input: InputStream, mediaType: Option[MediaType]): B = f(self.read(input, mediaType))
  }
}

trait BinaryEntityReader[A] extends EntityReader[A] {
  override final def read(input: InputStream, mediaType: Option[MediaType]): A =
    read(input)
  def read(input: InputStream): A
}

trait TextEntityReader[A] extends EntityReader[A] {
  def defaultCharset: Charset = DefaultCharset
  def read(input: Reader): A
  override final def read(input: InputStream, mediaType: Option[MediaType]): A =
    read(new InputStreamReader(input, (for(t <- mediaType; c <- t.charset) yield c).getOrElse(defaultCharset)))
}
