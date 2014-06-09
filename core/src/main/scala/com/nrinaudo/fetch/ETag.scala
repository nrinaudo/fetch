package com.nrinaudo.fetch

object ETag {
  private val TagPattern = """(W/)?"([^"]*)"""".r
  
  final case class Strong(value: String) extends ETag {
    override def toString = "\"" + value + "\""
    override def isWeak: Boolean = false
  }
  
  final case class Weak(value: String) extends ETag {
    override def toString = "W/\"" + value + "\""
    override def isWeak: Boolean = true
  }

  def parse(str: String): Option[ETag] = str match {
    case TagPattern(null, tag) => Some(Strong(tag))
    case TagPattern(_, tag)    => Some(Weak(tag))
    case _                     => None
  }

  def unapply[T](res: Response[T]): Option[ETag] = res.etag
}

/** Represents HTTP [[http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.11 entity tags]]. */
sealed trait ETag {
  def value: String
  def isWeak: Boolean
}
