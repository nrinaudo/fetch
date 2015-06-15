package com.nrinaudo.fetch

import java.nio.charset.Charset

import com.nrinaudo
import simulacrum._

import scala.annotation.implicitNotFound
import scala.util.Try


// - Value reading -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueReader {
  /** Transforms the specified function into an instance of [[ValueReader]]. */
  def apply[T](f: String => Option[T]): ValueReader[T] = new ValueReader[T] {
    override def read(value: String): Option[T] = f(value)
  }

  /** Acts as a [[ValueReader]] for sequences of the specified type.
    *
    * This method will yield an instance of `None` if at least one of the specified list's values is not legal.
    */
  def sequence[T: ValueReader](values: Seq[String]): Option[List[T]] =
    values.map(implicitly[ValueReader[T]].read).foldRight(Some(Nil): Option[List[T]]) {
      case (Some(v), Some(list)) => Some(v :: list)
      case _                     => None
    }


  // - Default readers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val Double: ValueReader[Double]       = ValueReader(s => Try(s.toDouble).toOption)
  implicit val Long: ValueReader[Long]           = ValueReader(s => Try(s.toLong).toOption)
  implicit val Short: ValueReader[Short]         = ValueReader(s => Try(s.toShort).toOption)
  implicit val Int: ValueReader[Int]             = ValueReader(s => Try(s.toInt).toOption)
  implicit val Byte: ValueReader[Byte]           = ValueReader(s => Try(s.toByte).toOption)
  implicit val Float: ValueReader[Float]         = ValueReader(s => Try(s.toFloat).toOption)
  implicit val Boolean: ValueReader[Boolean]     = ValueReader(s => Try(s.toBoolean).toOption)
  implicit val String: ValueReader[String]       = ValueReader(Some(_))
  implicit val Char: ValueReader[Char]           = ValueReader(s => if(s.length == 1) Some(s.charAt(0)) else None)
  implicit val Charset: ValueReader[Charset]     = ValueReader(c => Try(java.nio.charset.Charset.forName(c)).toOption)
  implicit val Language: ValueReader[Language]   = ValueReader(l => nrinaudo.fetch.Language.parse(l))
  implicit val Encoding: ValueReader[Encoding]   = ValueReader(e => nrinaudo.fetch.Encoding.DefaultEncodings.get(e))
  implicit val MediaType: ValueReader[MediaType] = ValueReader(m => nrinaudo.fetch.MediaType.parse(m))
  implicit val Method: ValueReader[Method]       = ValueReader(m => nrinaudo.fetch.Method.parse(m))
  implicit val ETag: ValueReader[ETag]           = ValueReader(e => nrinaudo.fetch.ETag.parse(e))
}

/** Used to read parameter values from an instance of [[Parameters]]. */
@implicitNotFound(msg = "Cannot find a ValueReader type class for ${T}")
@typeclass trait ValueReader[T] {
  /** Extract an instance of `T` from the specified value. */
  def read(value: String): Option[T]
}



// - Value Writing -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueWriter {
    /** Transforms the specified function into an instance of [[ValueWriter]]. */
  def apply[T](f: T => Option[String]): ValueWriter[T] = new ValueWriter[T] {
    override def write(value: T): Option[String] = f(value)
  }

  /** Acts as a [[ValueWriter]] for sequences of the specified type. */
  def sequence[T: ValueWriter](values: Seq[T]): Option[Seq[String]] =
    values.map(implicitly[ValueWriter[T]].write).collect {
      case Some(v) => v
    } match {
      case Nil => None
      case list => Some(list)
    }


  // - Default writers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val Double: ValueWriter[Double]       = ValueWriter(d => Some(d.toString))
  implicit val Long: ValueWriter[Long]           = ValueWriter(l => Some(l.toString))
  implicit val Short: ValueWriter[Short]         = ValueWriter(s => Some(s.toString))
  implicit val Int: ValueWriter[Int]             = ValueWriter(i => Some(i.toString))
  implicit val Byte: ValueWriter[Byte]           = ValueWriter(b => Some(b.toString))
  implicit val Float: ValueWriter[Float]         = ValueWriter(f => Some(f.toString))
  implicit val Boolean: ValueWriter[Boolean]     = ValueWriter(b => Some(b.toString))
  implicit val String: ValueWriter[String]       = ValueWriter(Some(_))
  implicit val Char: ValueWriter[Char]           = ValueWriter(b => Some(b.toString))
  implicit val Charset: ValueWriter[Charset]     = ValueWriter(c => Some(c.name()))
  implicit val Language: ValueWriter[Language]   = ValueWriter(l => Some(grammar.language(l.main, l.sub)))
  implicit val Encoding: ValueWriter[Encoding]   = ValueWriter(e => Some(e.name))
  implicit val MediaType: ValueWriter[MediaType] = ValueWriter(m => Some(m.toString))
  implicit val Method: ValueWriter[Method]       = ValueWriter(m => Some(m.name))
  implicit val ETag: ValueWriter[ETag]           = ValueWriter(e => Some(e.toString))
}

/** Used to write parameter values to an instance of [[Parameters]]. */
@implicitNotFound(msg = "Cannot find a ValueWriter type class for ${T}")
@typeclass trait ValueWriter[T] {
  /** Writes the specified `T` to a `String`. A return value of `None` means that the specified value serialises to
    * nothing (the empty string, `Nil`, ...).
    */
  def write(value: T): Option[String]
}



// - Formats -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueFormat {
  /** Transforms the specified functions into an instance of [[ValueFormat]]. */
  def apply[T](f: String => Option[T], g: T => Option[String]): ValueFormat[T] = apply(ValueReader(f), ValueWriter(g))

  /** Transforms the specified [[ValueReader]] and [[ValueWriter]] into an instance of [[ValueFormat]]. */
  def apply[T](reader: ValueReader[T], writer: ValueWriter[T]): ValueFormat[T] = new ValueFormat[T] {
    override def write(value: T): Option[String] = writer.write(value)
    override def read(value: String): Option[T] = reader.read(value)
  }
}

/** Compounds a [[ValueReader]] and [[ValueWriter]]. */
trait ValueFormat[T] extends ValueReader[T] with ValueWriter[T]