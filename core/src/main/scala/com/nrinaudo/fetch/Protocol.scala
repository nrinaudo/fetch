package com.nrinaudo.fetch

import java.util.Locale

/** Declares known protocols and provides convenience methods. */
object Protocol {
  /** Underlying implementation. */
  private case class ProtocolImpl(name: String, defaultPort: Int) extends Protocol {
    override def toString = name
  }

  /** The HTTP protocol. */
  val Http: Protocol  = new ProtocolImpl("http", 80)
  /** The HTTPs protocol. */
  val Https: Protocol = new ProtocolImpl("https", 443)

  def parse(str: String): Option[Protocol] = str.toLowerCase(Locale.ENGLISH) match {
    case Http.name  => Some(Http)
    case Https.name => Some(Https)
    case _          => None
  }
}

/** Represents a valid URL protocol.
  *
  * Supported protocols are declared in the [[Protocol$ companion object]].
  *
  * `Protocol` instances can be used to easily create [[Url urls]]:
  * {{{
  * // Verbose:
  * Protocol.Http.host("github.com")
  *
  * // Syntactic sugar:
  * Protocol.Http :/ "github.com"
  * }}}
  */
sealed trait Protocol {
  /** Name of the protocol as used in the URL string. */
  val name: String

  /** Default port associated with this protocol. */
  val defaultPort: Int

  /** Creates a new [[Url]] using on the specified host using this protocol.
    *
    * The resulting url will use the default port and have an empty path, query string and fragment.
    */
  def host(name: String): Url = new Url(this, name, defaultPort)

  /** Syntactic sugar for [[host]]. It unfortunately can't be `://`, as this isn't a legal scala identifier. */
  def :/(name: String): Url = host(name)
}
