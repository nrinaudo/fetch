package com.nrinaudo.fetch

import scala.util.{Failure, Success, Try}

object ValueReader {
  def apply[T](f: String => Try[T]): ValueReader[T] = new ValueReader[T] {
    override def read(value: String): Try[T] = f(value)
  }

  def sequence[T: ValueReader](values: Seq[String]): Try[List[T]] =
    values.map(implicitly[ValueReader[T]].read).foldRight(Success(Nil): Try[List[T]]) {
      case (Success(v), Success(list)) => Success(v :: list)
      case (_, acc @ Failure(_))       => acc
      case (Failure(e),             _) => Failure(e)
    }
}

trait ValueReader[T] {
  def read(value: String): Try[T]
}

object ValueWriter {
  def apply[T](f: T => Option[String]): ValueWriter[T] = new ValueWriter[T] {
    override def write(value: T): Option[String] = f(value)
  }

  def sequence[T: ValueWriter](values: Seq[T]): Option[Seq[String]] =
    values.map(implicitly[ValueWriter[T]].write).collect {
      case Some(v) => v
    } match {
      case Nil => None
      case list => Some(list)
    }
}

trait ValueWriter[T] {
  def write(value: T): Option[String]
}

object ValueFormat {
  // - Standard formatters ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val Doubles: ValueFormat[Double]   = apply(s => Try {s.toDouble},  d => Some {d.toString})
  val Longs: ValueFormat[Long]       = apply(s => Try {s.toLong},    l => Some {l.toString})
  val Shorts: ValueFormat[Short]     = apply(s => Try {s.toShort},   s => Some {s.toString})
  val Ints: ValueFormat[Int]         = apply(s => Try {s.toInt},     i => Some {i.toString})
  val Bytes: ValueFormat[Byte]       = apply(s => Try {s.toByte},    b => Some {b.toString})
  val Floats: ValueFormat[Float]     = apply(s => Try {s.toFloat},   f => Some {f.toString})
  val Booleans: ValueFormat[Boolean] = apply(s => Try {s.toBoolean}, b => Some {b.toString})
  val Strings: ValueFormat[String]   = apply(Success(_),             Some(_))

  def apply[T](f: String => Try[T], g: T => Option[String]): ValueFormat[T] = apply(ValueReader(f), ValueWriter(g))

  def apply[T](reader: ValueReader[T], writer: ValueWriter[T]): ValueFormat[T] = new ValueFormat[T] {
    override def write(value: T): Option[String] = writer.write(value)
    override def read(value: String): Try[T] = reader.read(value)
  }
}

trait ValueFormat[T] extends ValueReader[T] with ValueWriter[T]