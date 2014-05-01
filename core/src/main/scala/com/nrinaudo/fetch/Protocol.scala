package com.nrinaudo.fetch

/** Known protocols. */
object Protocol {
  object Http extends Protocol("http", Some(80))
  object Https extends Protocol("https", Some(443))

  def unapply(str: String): Option[Protocol] = str match {
    case Http.name  => Some(Http)
    case Https.name => Some(Https)
    case _          => Some(Protocol(str, None))
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
  * @param defaultPort default port associated with this protocol, if any.
  */
case class Protocol(name: String, defaultPort: Option[Int])