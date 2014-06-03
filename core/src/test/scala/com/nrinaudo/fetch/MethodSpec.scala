package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object MethodSpec {
  /** Generates random, legal HTTP methods. */
  def httpMethod = Gen.oneOf(Method.GET, Method.POST, Method.PUT, Method.DELETE, Method.OPTIONS, Method.TRACE,
    Method.PATCH, Method.LINK, Method.UNLINK)

  def illegalMethod: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.exists(!_.isLetter))

  def httpMethods = for {
    count <- Gen.choose(1, 5)
    set   <- Gen.containerOfN[Set, Method](count, httpMethod)
  } yield set.toList
}

class MethodSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MethodSpec._

  describe("The Method companion object") {
    it("should parse known methods") {
      forAll(httpMethod) { method => Method.parse(method.name) should be(Some(method)) }
    }

    it("should apply on known mehods") {
      forAll(httpMethod) { method => Method(method.name) should be(method) }
    }

    it("should parse unknown but legal HTTP methods") {
      forAll(Gen.alphaStr.suchThat(!_.isEmpty)) { method => Method.parse(method) should be(Some(Method(method))) }
    }

    it("should not parse illegal HTTP methods") {
      forAll(illegalMethod) { method => Method.parse(method) should be(None) }
    }

    it("should fail to apply on illegal HTTP methods") {
      forAll(illegalMethod) { method => intercept[IllegalArgumentException](Method(method)) }
    }
  }

  describe("A Method instance") {
    it("should serialize to itself") {
      forAll(httpMethod) { method => Method(method.toString) should be(method) }
    }
  }
}
