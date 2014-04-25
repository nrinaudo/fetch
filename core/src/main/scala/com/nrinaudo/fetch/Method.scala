package com.nrinaudo.fetch

import com.nrinaudo.fetch.Method.GET

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
}

/**
 * Represents a valid HTTP method.
 *
 * See the [[Method$ companion object]] for standard values.
 */
case class Method(name: String)