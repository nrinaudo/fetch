package com.nrinaudo.fetch

import scala.util.Try

trait Parameters[S <: Parameters[S]] {
  this: S =>

  val values: Map[String, String]

  def build(values: Map[String, String]): S

  def apply[T: ValueReader](name: String): T = implicitly[ValueReader[T]].read(values(name)).get

  def getOpt[T: ValueReader](name: String): Option[T] = for {
    raw    <- values.get(name)
    parsed <- implicitly[ValueReader[T]].read(raw).toOption
  } yield parsed

  def get[T: ValueReader](name: String): Option[Try[T]] = values.get(name) map implicitly[ValueReader[T]].read

  def set[T: ValueWriter](name: String, value: T): S =
    implicitly[ValueWriter[T]].write(value).fold(this)(set(name, _))

  def set(name: String, value: String): S = build(values + (name -> value))

  def setIfEmpty[T: ValueWriter](name: String, value: T): S =
    if(contains(name)) this
    else               set(name, value)

  def remove(name: String): S =
    if(contains(name)) build(values - name)
    else               this

  def contains(name: String): Boolean = values.contains(name)

  override def toString = values.mkString("(", ",", ")")

  override def hashCode(): Int = values.hashCode()

  override def equals(p1: scala.Any): Boolean = p1 match {
    case a: Parameters[_] => a.values == values
    case _                => false
  }
}