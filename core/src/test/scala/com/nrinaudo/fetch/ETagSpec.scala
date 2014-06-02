package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object ETagSpec {
  def weakTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield WeakTag(tag)
  def strongTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield StrongTag(tag)

  def etag: Gen[ETag] = Gen.oneOf(weakTag, strongTag)

  def etags = HeadersSpec.headers(etag)

  def invalidEtag = Arbitrary.arbitrary[String].suchThat { str =>
    str.length == 0 || str.charAt(0) != 'W' || str.charAt(0) != '\"' || str.charAt(str.length - 1) != '\"'
  }
}

class ETagSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ETagSpec._

  describe("The ETag companion object") {
    it("should parse valid etags") {
      forAll(etag) { etag => ETag.parse(etag.toString) should be(Some(etag)) }
    }

    it("should not parse invalid etags") {
      forAll(invalidEtag) { str => ETag.parse(str) should be(None) }
    }
  }

  describe("An ETag instance") {
    it("should have the correct weak flag") {
      forAll(etag) {
        case etag @ WeakTag(_)   => etag.isWeak should be(true)
        case etag @ StrongTag(_) => etag.isWeak should be(false)
      }
    }
  }
}
