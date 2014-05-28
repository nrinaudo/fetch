package com.nrinaudo.fetch

object ByteRange {
  private val Extractor = """([0-9]+)?-([0-9]+)?""".r

  def unapply(str: String): Option[ByteRange] = str match {
    case Extractor(prefix, null)   => Some(PrefixRange(prefix.toInt))
    case Extractor(null,   suffix) => Some(SuffixRange(suffix.toInt))
    case Extractor(prefix, suffix) => Some(FullRange(prefix.toInt, suffix.toInt))
    case _                         => None
  }

  def apply(str: String): ByteRange = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal byte range: " + str)
  }
}

sealed trait ByteRange

case class SuffixRange(to: Int) extends ByteRange {
  require(to >= 0)

  override def toString = "-%d" format to
}

case class PrefixRange(from: Int) extends ByteRange {
  require(from >= 0)

  override def toString = "%d-" format from
}

case class FullRange(from: Int, to: Int) extends ByteRange {
  require(from >= 0 && to >= from)

  override def toString = "%d-%d" format(from, to)
}