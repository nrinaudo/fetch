package com.nrinaudo.fetch

import java.util.Locale

object Method {
  val GET: Method     = MethodImpl("GET")
  val POST: Method    = MethodImpl("POST")
  val PUT: Method     = MethodImpl("PUT")
  val DELETE: Method  = MethodImpl("DELETE")
  val HEAD: Method    = MethodImpl("HEAD")
  val OPTIONS: Method = MethodImpl("OPTIONS")
  val TRACE: Method   = MethodImpl("TRACE")
  val CONNECT: Method = MethodImpl("CONNECT")
  val PATCH: Method   = MethodImpl("PATCH")
  val LINK: Method    = MethodImpl("LINK")
  val UNLINK: Method  = MethodImpl("UNLINK")

  private case class MethodImpl(name: String) extends Method

  private val MethodPattern = """(\p{Alpha}+)""".r

  def apply(value: String): Method = parse(value) getOrElse {throw new IllegalArgumentException("Not a valid method: " + value)}

  def unapply(value: Method): Option[String] = Some(value.name)

  def parse(value: String): Option[Method] = value match {
    case MethodPattern(m) => Some(MethodImpl(m.toUpperCase(Locale.ENGLISH)))
    case _                => None
  }
}

sealed trait Method {
  val name: String
  override def toString = name
}
