package com.nrinaudo.fetch

object Method {
  object GET extends Method("GET")
  object POST extends Method("POST")
  object PUT extends Method("PUT")
  object DELETE extends Method("DELETE")
  object HEAD extends Method("HEAD")
  object OPTIONS extends Method("OPTIONS")
  object TRACE extends Method("TRACE")
  object CONNECT extends Method("CONNECT")
  object PATCH extends Method("PATCH")
  object LINK extends Method("LINK")
  object UNLINK extends Method("UNLINK")

  def unapply(value: String): Option[Method] = value match {
    case GET.name                        => Some(GET)
    case POST.name                       => Some(POST)
    case PUT.name                        => Some(PUT)
    case DELETE.name                     => Some(DELETE)
    case HEAD.name                       => Some(HEAD)
    case OPTIONS.name                    => Some(OPTIONS)
    case TRACE.name                      => Some(TRACE)
    case CONNECT.name                    => Some(CONNECT)
    case PATCH.name                      => Some(PATCH)
    case LINK.name                       => Some(LINK)
    case UNLINK.name                     => Some(UNLINK)
    case str if !str.matches(".*\\s+.*") => Some(Method(str))
    case _                               => None
  }
}

/**
 * Represents a valid HTTP method.
 *
 * See the [[Method$ companion object]] for standard values.
 */
case class Method(name: String)