package fetch

import java.util.Locale

/** Declares known protocols and provides convenience methods. */
object Protocol {
  /** The HTTP protocol. */
  case object Http extends Protocol("http", 80)
  /** The HTTPs protocol. */
  case object Https extends Protocol("https", 443)

  def parse(str: String): Option[Protocol] = str.toLowerCase(Locale.ENGLISH) match {
    case Http.name  => Some(Http)
    case Https.name => Some(Https)
    case a          => None
  }

  def unapply(protocol: Protocol): Option[(String, Int)] = Some((protocol.name, protocol.defaultPort))
}

/** Represents a valid URL protocol.
  *
  * Supported protocols are declared in the [[Protocol$ companion object]].
  *
  * `Protocol` instances can be used to easily create [[Url urls]]:
  * {{{Protocol.Http.host("github.com")}}}
  */
sealed abstract class Protocol(val name: String, val defaultPort: Int) {
  /** Creates a new [[Url]] on the specified host using this protocol.
    *
    * The resulting url will use the default port and have an empty path, query string and fragment.
    */
  def host(name: String): Url = new Url(this, name, defaultPort)
}
