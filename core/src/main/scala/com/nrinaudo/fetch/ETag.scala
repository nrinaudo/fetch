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

  def unapply(str: String): Option[ETag] = str match {
    case TagPattern(null, tag) => Some(Strong(tag))
    case TagPattern(_, tag)    => Some(Weak(tag))
    case _                     => None
  }

  def apply(str: String): ETag = unapply(str) getOrElse {throw new IllegalArgumentException("Illegal ETag: " + str)}
}

sealed trait ETag {
  def value: String
  def isWeak: Boolean
}
