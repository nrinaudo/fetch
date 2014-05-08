package com.nrinaudo.fetch

import scala.util.Try

trait ValueReader[S, T] {
  def read(value: S): Try[T]
}

trait ValueWriter[S, T] {
  def write(value: S): Option[T]
}

class KeyValueStore[T](val values: Map[String, T] = Map()) {
  def apply[S](name: String)(implicit reader: ValueReader[T, S]): S =
    reader.read(values(name)).get

  def getOpt[S](name: String)(implicit reader: ValueReader[T, S]): Option[S] = for {
    raw    <- values.get(name)
    parsed <- reader.read(raw).toOption
  } yield parsed

  def get[S](name: String)(implicit reader: ValueReader[T, S]): Option[Try[S]] = values.get(name) map reader.read

  def set[S](name: String, value: S)(implicit writer: ValueWriter[S, T]): KeyValueStore[T] =
    writer.write(value).fold(this)(set(name, _))

  def set(name: String, value: T): KeyValueStore[T] = new KeyValueStore(values + (name -> value))

  def setIfEmpty[S](name: String, value: S)(implicit writer: ValueWriter[S, T]): KeyValueStore[T] =
      if(contains(name)) this
      else               set(name, value)

  def remove(name: String): KeyValueStore[T] =
      if(contains(name)) new KeyValueStore[T](values - name)
      else               this

  def contains(name: String): Boolean = values.contains(name)
}
