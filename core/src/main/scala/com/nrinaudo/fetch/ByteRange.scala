package com.nrinaudo.fetch

// TODO: constructors are probably not ok - throwing exceptions in constructors is frowned upon.

import scala.util.Try

object ByteRange {
  private val Extractor = """([0-9]+)?-([0-9]+)?""".r

  private object PositiveInt {
    def unapply(value: String): Option[Int] = Try(value.toInt).toOption.filter(_ >= 0)
  }

  def parse(str: String): Option[ByteRange] = str match {
    case Extractor(PositiveInt(prefix), null)   => Some(PrefixRange(prefix.toInt))
    case Extractor(null,   PositiveInt(suffix)) => Some(SuffixRange(suffix.toInt))
    case Extractor(PositiveInt(prefix), PositiveInt(suffix)) if suffix >= prefix =>
      Some(FullRange(prefix.toInt, suffix.toInt))
    case _ => None
  }
}

sealed trait ByteRange

final case class SuffixRange(to: Int) extends ByteRange {
  require(to >= 0)

  override def toString = "-%d" format to
}

final case class PrefixRange(from: Int) extends ByteRange {
  require(from >= 0)

  override def toString = "%d-" format from
}

final case class FullRange(from: Int, to: Int) extends ByteRange {
  require(from >= 0 && to >= from)

  override def toString = "%d-%d" format(from, to)
}