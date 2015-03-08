package com.nrinaudo.fetch

import java.nio.charset.Charset

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
}

/** Used to read parameter values from an instance of [[Parameters]]. */
@implicitNotFound(msg = "Cannot find a ValueReader type class for ${T}")
trait ValueReader[T] {
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
}

/** Used to write parameter values to an instance of [[Parameters]]. */
@implicitNotFound(msg = "Cannot find a ValueWriter type class for ${T}")
trait ValueWriter[T] {
  /** Writes the specified `T` to a `String`. A return value of `None` means that the specified value serialises to
    * nothing (the empty string, `Nil`, ...).
    */
  def write(value: T): Option[String]
}



// - Formats -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueFormat {
  // - Standard formatters ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // TODO: Should empty strings format as None by default?
  val Doubles: ValueFormat[Double]   = apply(s => Try(s.toDouble).toOption,  d => Some {d.toString})
  val Longs: ValueFormat[Long]       = apply(s => Try(s.toLong).toOption,    l => Some {l.toString})
  val Shorts: ValueFormat[Short]     = apply(s => Try(s.toShort).toOption,   s => Some {s.toString})
  val Ints: ValueFormat[Int]         = apply(s => Try(s.toInt).toOption,     i => Some {i.toString})
  val Bytes: ValueFormat[Byte]       = apply(s => Try(s.toByte).toOption,    b => Some {b.toString})
  val Floats: ValueFormat[Float]     = apply(s => Try(s.toFloat).toOption,   f => Some {f.toString})
  val Booleans: ValueFormat[Boolean] = apply(s => Try(s.toBoolean).toOption, b => Some {b.toString})
  val Strings: ValueFormat[String]   = apply(Some(_),                        Some(_))
  val Chars: ValueFormat[Char]       = apply(s => if(s.length == 1) Some(s.charAt(0)) else None, b => Some(b.toString))
  val Charsets: ValueFormat[Charset] = new ValueFormat[Charset] {
    override def write(value: Charset): Option[String] = Some(value.name())
    override def read(value: String): Option[Charset] = Try(Charset.forName(value)).toOption
  }

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