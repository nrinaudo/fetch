package com.nrinaudo.fetch

import com.nrinaudo.fetch.Generators._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class MethodSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("The Method companion object") {
    it("should parse known methods") {
      forAll { method: Method => Method.parse(method.name) should be(Some(method)) }
    }

    it("should apply on known methods") {
      forAll { method: Method => Method(method.name) should be(method) }
    }

    it("should parse unknown but legal HTTP methods") {
      forAll(Gen.alphaStr.suchThat(!_.isEmpty)) { method => Method.parse(method) should be(Some(Method(method))) }
    }

    it("should not parse illegal HTTP methods") {
      forAll(illegalMethod) { method => Method.parse(method) should be(None) }
    }

    it("should fail to apply on illegal HTTP methods") {
      forAll(illegalMethod) { method => intercept[IllegalArgumentException](Method(method)); () }
    }
  }

  describe("A Method instance") {
    it("should serialize to itself") {
      forAll { method: Method => Method(method.toString) should be(method) }
    }

    it("should unapply as expected") {
      forAll { method: Method =>
        val Method(name) = method
        method.name should be(name)
      }
    }
  }
}
