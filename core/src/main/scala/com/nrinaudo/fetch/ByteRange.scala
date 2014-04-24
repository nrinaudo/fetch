package com.nrinaudo.fetch

import scala.util.Try

object ByteRange {
  private val Extractor = """([0-9]+)?-([0-9]+)?""".r

  def unapply(str: String): Option[ByteRange] = str match {
    case Extractor(left, right) => Try {ByteRange(Option(left).map {_.toInt}, Option(right).map {_.toInt})}.toOption
    case _                      => None
  }
}

/** Represents an acceptable value for the `Range` HTTP header. */
case class ByteRange(from: Option[Int], to: Option[Int]) {
  // Sanity checks.
  require(from.isDefined || to.isDefined, "At least one boundary of the range must be defined")
  require(!from.isDefined || from.get >= 0, "When defined, the lower boundary must be greater than or equal to 0")
  require(!to.isDefined || to.get >= 0, "When defined, the upper boundary must be greater than or equal to 0")
  require(!(from.isDefined && to.isDefined) || to.get >= from.get,
    "When both defined, the upper boundary must be greater than or equal to the lower one")

  /** Represents this instance as a `[from]-[to]` string. */
  override def toString = {
    val builder = new StringBuilder

    from.foreach(builder.append)
    builder.append('-')
    to.foreach(builder.append)

    builder.result()
  }
}