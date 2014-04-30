package com.nrinaudo.fetch

object ETag {
  private val TagPattern = """(W/)?"([^"]*)"""".r

  def unapply(str: String): Option[ETag] = str match {
    case TagPattern(null, tag) => Some(StrongTag(tag))
    case TagPattern(_, tag)    => Some(WeakTag(tag))
    case _                     => None
  }

  def apply(str: String): ETag = unapply(str) getOrElse {throw new IllegalArgumentException("Illegal ETag: " + str)}
}

sealed trait ETag {
  def value: String
  require(!value.isEmpty)
}

case class StrongTag(value: String) extends ETag {
  override def toString = "\"" + value + "\""
}

case class WeakTag(value: String) extends ETag {
  override def toString = "W/\"" + value + "\""
}