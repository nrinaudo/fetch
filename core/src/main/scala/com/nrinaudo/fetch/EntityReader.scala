package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

import simulacrum.typeclass

import scala.annotation.implicitNotFound
import scala.io.Codec

object EntityReader {
  /** Creates a new [[EntityReader]] that reads data from a byte stream. */
  def bytes[A](f: InputStream => A): EntityReader[A] = new EntityReader[A] {
    override def read(input: InputStream, mediaType: Option[MediaType]): A = f(input)
  }

  /** Creates a new [[EntityReader]] that reads data from a character stream. */
  def chars[A](f: Reader => A)(implicit defaultCharset: Charset): EntityReader[A] = new EntityReader[A] {
    override def read(input: InputStream, mediaType: Option[MediaType]): A =
      f(new InputStreamReader(input, (for(t <- mediaType; c <- t.charset) yield c).getOrElse(defaultCharset)))
  }

  /** [[EntityReader]] implementation that turns an entity body into a `String`. */
  implicit def stringReader(implicit defaultCharset: Charset): EntityReader[String] = chars { input =>
    val writer = new StringWriter()
    writeChars(input, writer)
    writer.toString
  }

  implicit val intReader: EntityReader[Int]         = stringReader(DefaultCharset).map(_.toInt)
  implicit val floatReader: EntityReader[Float]     = stringReader(DefaultCharset).map(_.toFloat)
  implicit val doubleReader: EntityReader[Double]   = stringReader(DefaultCharset).map(_.toDouble)
  implicit val longReader: EntityReader[Long]       = stringReader(DefaultCharset).map(_.toLong)
  implicit val booleanReader: EntityReader[Boolean] = stringReader(DefaultCharset).map(_.toBoolean)

  val ignoreReader: EntityReader[Unit] = bytes(_ => ())
  val emptyReader: EntityReader[Unit] = bytes(writeBytes(_, new OutputStream {
    override def write(b: Int): Unit = {}
  }))
}

/** Type class used to turn an entity body into something more useful.
  *
  * There are two types of entity bodies: binary and text ones. Binary entities are composed of raw bytes, while text
  * ones are composed of characters.
  *
  * Instances of [[EntityReader]] are typically created through the companion's object helper methods.
  */
@implicitNotFound(msg = "Cannot find an EntityReader instance for ${A}")
@typeclass trait EntityReader[A] { self =>
  def read(input: InputStream, mediaType: Option[MediaType]): A

  def map[B](f: A => B): EntityReader[B] = new EntityReader[B] {
    override def read(input: InputStream, mediaType: Option[MediaType]): B = f(self.read(input, mediaType))
  }
}