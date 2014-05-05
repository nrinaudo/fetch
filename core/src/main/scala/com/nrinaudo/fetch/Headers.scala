package com.nrinaudo.fetch

import scala.util.{Failure, Try}

/** Represents the headers associated with an HTTP request or response. */
class Headers(val values: Map[String, String] = Map()) {
  // - Generic header setting / getting --------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def apply[T: HeaderReader](name: String): T = {
    val raw = values(name)
    implicitly[HeaderReader[T]].read(raw).get
  }

  def getOpt[T: HeaderReader](name: String): Option[T] = for {
    raw    <- values.get(name)
    parsed <- implicitly[HeaderReader[T]].read(raw).toOption
  } yield parsed

  def get[T: HeaderReader](name: String): Option[Try[T]] = values.get(name) map {implicitly[HeaderReader[T]].read}

  def set[T: HeaderWriter](name: String, value: T): Headers = {
    val formatted = implicitly[HeaderWriter[T]].write(value)

    if(formatted.isEmpty) this
    else                  new Headers(values + (name -> formatted))
  }

  def setIfEmpty[T: HeaderFormat](name: String, value: T): Headers =
    if(contains(name)) this
    else               set(name, value)

  def remove(name: String): Headers =
    if(values.contains(name)) new Headers(values - name)
    else                      this

  def contains(name: String): Boolean = values.contains(name)
}
