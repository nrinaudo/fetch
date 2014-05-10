package com.nrinaudo.fetch

import scala.util.{Failure, Success, Try}


// - Value parsing -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueReader {
  def apply[S, T](f: S => Try[T]): ValueReader[S, T] = new ValueReader[S, T] {
    override def read(value: S): Try[T] = f(value)
  }

  def seq[S, T](reader: ValueReader[S, T]): ValueReader[Seq[S], Seq[T]] = new ValueReader[Seq[S], Seq[T]] {
    override def read(value: Seq[S]): Try[Seq[T]] = value.map(reader.read).foldRight(Success(Nil): Try[List[T]]) {
      case (Success(v), Success(list)) => Success(v :: list)
      case (_, acc @ Failure(_))       => acc
      case (Failure(e),             _) => Failure(e)
    }
  }
}

trait ValueReader[S, T] {
  def read(value: S): Try[T]
  def andThen[U](f: T => Try[U]): ValueReader[S, U] = andThen(ValueReader(f))
  def andThen[U](reader: ValueReader[T, U]): ValueReader[S, U] = ValueReader {s: S => read(s) flatMap reader.read}
}



// - Value formatting --------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueWriter {
  def apply[S, T](f: S => Option[T]): ValueWriter[S, T] = new ValueWriter[S, T] {
    override def write(value: S): Option[T] = f(value)
  }

  def seq[S, T](writer: ValueWriter[S, T]): ValueWriter[Seq[S], Seq[T]] = new ValueWriter[Seq[S], Seq[T]] {
    override def write(value: Seq[S]): Option[Seq[T]] = value.map(writer.write).collect {
      case Some(v) => v
    } match {
      case Nil  => None
      case list => Some(list)
    }
  }
}

trait ValueWriter[S, T] {
  def write(value: S): Option[T]
  def andThen[U](f: T => Option[U]): ValueWriter[S, U] = andThen(ValueWriter(f))
  def andThen[U](writer: ValueWriter[T, U]): ValueWriter[S, U] = ValueWriter {s: S => write(s) flatMap writer.write}
}



// - Helpers -----------------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object ValueFormat {
  def apply[S, T](reader: ValueReader[S, T], writer: ValueWriter[T, S]): ValueFormat[S, T] = new ValueFormat[S, T] {
    override def read(value: S): Try[T] = reader.read(value)
    override def write(value: T): Option[S] = writer.write(value)
  }

  def apply[S, T](reader: S => Try[T], writer: T => Option[S]): ValueFormat[S, T] =
    apply(ValueReader(reader), ValueWriter(writer))
}

trait ValueFormat[S, T] extends ValueReader[S, T] with ValueWriter[T, S]



// - Value storage -----------------------------------------------------------------------------------------------------
// ---------------------------------------------------------------------------------------------------------------------
object KeyValueStore {
  object StringFormats {
    val Ints: ValueFormat[String, Int] = ValueFormat((s) => Try {s.toInt}, (i) => Some {i.toString})
    val Strings: ValueFormat[String, String] = ValueFormat(Success(_), Some(_))
  }
}

trait KeyValueStore[S, T <: KeyValueStore[S, T]] {
  this: T =>
  val values: Map[String, S]
  def build(values: Map[String, S]): T

  def apply[U](name: String)(implicit reader: ValueReader[S, U]): U =
    reader.read(values(name)).get

  def getOpt[U](name: String)(implicit reader: ValueReader[S, U]): Option[U] = for {
    raw    <- values.get(name)
    parsed <- reader.read(raw).toOption
  } yield parsed

  def get[U](name: String)(implicit reader: ValueReader[S, U]): Option[Try[U]] = values.get(name) map reader.read

  def set[U](name: String, value: U)(implicit writer: ValueWriter[U, S]): T =
    writer.write(value).fold(this)(set(name, _))

  def set(name: String, value: S): T = build(values + (name -> value))

  def setIfEmpty[U](name: String, value: U)(implicit writer: ValueWriter[U, S]): T =
      if(contains(name)) this
      else               set(name, value)

  def remove(name: String): T =
      if(contains(name)) build(values - name)
      else               this

  def contains(name: String): Boolean = values.contains(name)
}