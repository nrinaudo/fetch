package com.nrinaudo.fetch

// TODO: since we're only allowing Http and Https, it'd probably make more sense for Protocol to be an enumeration.

/** Known protocols. */
object Protocol {
  object Http extends Protocol("http", 80)
  object Https extends Protocol("https", 443)

  def unapply(str: String): Option[Protocol] = str match {
    case Http.name  => Some(Http)
    case Https.name => Some(Https)
    case _          => None
  }

  def apply(str: String): Protocol = unapply(str) getOrElse {
    throw new IllegalArgumentException("Illegal protocol: " + str)
  }
}

/** Represents a valid URL protocol.
  *
  * See the [[Protocol$ companion object]] for standard instances.
  *
  * @param name        name of the protocol as used in URL strings.
  * @param defaultPort default port associated with this protocol.
  */
case class Protocol(name: String, defaultPort: Int) {
  /** Creates a new [[Url]] using on the specified host using this protocol. */
  def host(name: String): Url = new Url(this, name, defaultPort)

  /** Syntactic sugar for [[host]]. It unfortunately can't be `://`, as this isn't a legal scala identifier. */
  def :/(name: String): Url = host(name)
}