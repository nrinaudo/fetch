package com.nrinaudo.fetch

import java.io._
import java.nio.charset.Charset

import scala.annotation.implicitNotFound

object EntityReader {
  /** Creates a new {{{EntityReader}}} that reads data from a byte stream. */
  def bytes[A](f: InputStream => A): EntityReader[A] = new BinaryEntityReader[A] {
    override def read(input: InputStream): A = f(input)
  }

  /** Creates a new {{{EntityReader}}} that reads data from a character stream. */
  def chars[A](f: Reader => A): EntityReader[A] = new TextEntityReader[A] {
    override def read(input: Reader): A = f(input)
  }

  /** {{{EntityReader}}} implementation that turns an entity body into a [[String]]. */
  implicit val stringReader: EntityReader[String] = chars { input =>
    val writer = new StringWriter()
    writeChars(input, writer)
    writer.toString
  }

  implicit val intReader: EntityReader[Int]         = stringReader.map(_.toInt)
  implicit val floatReader: EntityReader[Float]     = stringReader.map(_.toFloat)
  implicit val doubleReader: EntityReader[Double]   = stringReader.map(_.toDouble)
  implicit val longReader: EntityReader[Long]       = stringReader.map(_.toLong)
  implicit val booleanReader: EntityReader[Boolean] = stringReader.map(_.toBoolean)

  val ignoreReader: EntityReader[Unit] = bytes(_ => ())
  val emptyReader: EntityReader[Unit] = bytes(writeBytes(_, new OutputStream {
    override def write(b: Int): Unit = {}
  }))
}

/** Typeclass used to turn an entity body into something more useful.
  *
  * There are two types of entity bodies: binary and text ones. Binary entities are composed of raw bytes, while text
  * ones are composed of characters.
  *
  * Instances of {{{EntityReader}}} are typically created through the companion's object helper methods.
  */
@implicitNotFound(msg = "Cannot find an EntityReader typeclass for ${A}")
sealed trait EntityReader[A] { self =>
  def read(input: InputStream, mediaType: Option[MediaType]): A

  def map[B](f: A => B): EntityReader[B] = new EntityReader[B] {
    override def read(input: InputStream, mediaType: Option[MediaType]): B = f(self.read(input, mediaType))
  }
}

/** Implementations of this trait are used to turn a binary entity body into a more useful type. */
trait BinaryEntityReader[A] extends EntityReader[A] {
  override final def read(input: InputStream, mediaType: Option[MediaType]): A =
    read(input)

  /** Turns the specified stream of bytes into a valid instance of ${A}.
    *
    * The stream parameter is managed by the caller and should not be closed by implementations.
    */
  def read(input: InputStream): A
}

/** Implementations of this trait are used to turn a text entity body into a more useful type. */
trait TextEntityReader[A] extends EntityReader[A] {
  def defaultCharset: Charset = DefaultCharset

  override final def read(input: InputStream, mediaType: Option[MediaType]): A =
    read(new InputStreamReader(input, (for(t <- mediaType; c <- t.charset) yield c).getOrElse(defaultCharset)))

  /** Turns the specified stream of characters into a valid instance of ${A}.
    *
    * The stream parameter is managed by the caller and should not be closed by implementations.
    */
  def read(input: Reader): A
}
