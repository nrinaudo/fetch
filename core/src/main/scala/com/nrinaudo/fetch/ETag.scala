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

final case class StrongTag(value: String) extends ETag {
  override def toString = "\"" + value + "\""
  override def isWeak: Boolean = false
}

final case class WeakTag(value: String) extends ETag {
  override def toString = "W/\"" + value + "\""
  override def isWeak: Boolean = true
}

/** Represents HTTP [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.11 entity tags]]. */
sealed trait ETag {
  def value: String
  def isWeak: Boolean
}
