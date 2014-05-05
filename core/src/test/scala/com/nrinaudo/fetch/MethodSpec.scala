package com.nrinaudo.fetch

import org.scalacheck.Gen

object MethodSpec {
  /** Generates random, legal HTTP methods. */
    def httpMethod = Gen.oneOf(Method.GET, Method.POST, Method.PUT, Method.DELETE, Method.OPTIONS, Method.TRACE,
      Method.PATCH, Method.LINK, Method.UNLINK)

  def httpMethods = for {
    count <- Gen.choose(1, 5)
    set   <- Gen.containerOfN[Set, Method](count, httpMethod)
  } yield set.toList
}
