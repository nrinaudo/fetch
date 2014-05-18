package com.nrinaudo.fetch

import org.scalacheck.Gen
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object MethodSpec {
  /** Generates random, legal HTTP methods. */
  def httpMethod = Gen.oneOf(Method.GET, Method.POST, Method.PUT, Method.DELETE, Method.OPTIONS, Method.TRACE,
    Method.PATCH, Method.LINK, Method.UNLINK)

  def httpMethods = for {
    count <- Gen.choose(1, 5)
    set   <- Gen.containerOfN[Set, Method](count, httpMethod)
  } yield set.toList
}

class MethodSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("Method") {
    it("should unapply against known HTTP methods") {
      Method.unapply("GET") should be(Some(Method.GET))
      Method.unapply("POST") should be(Some(Method.POST))
      Method.unapply("PUT") should be(Some(Method.PUT))
      Method.unapply("DELETE") should be(Some(Method.DELETE))
      Method.unapply("HEAD") should be(Some(Method.HEAD))
      Method.unapply("OPTIONS") should be(Some(Method.OPTIONS))
      Method.unapply("TRACE") should be(Some(Method.TRACE))
      Method.unapply("CONNECT") should be(Some(Method.CONNECT))
      Method.unapply("PATCH") should be(Some(Method.PATCH))
      Method.unapply("LINK") should be(Some(Method.LINK))
      Method.unapply("UNLINK") should be(Some(Method.UNLINK))
    }

    it("should unapply against unknown but legal HTTP methods") {
      forAll(Gen.alphaStr.suchThat(!_.isEmpty)) { method =>
        Method.unapply(method) should be(Some(Method(method)))
      }
    }

    it("should not unapply against illegal HTTP methods") {
      forAll(Gen.identifier, Gen.identifier) { (a, b) =>
        Method.unapply(a + ' ' + b) should be(None)
      }
    }
  }
}
