package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object ETagSpec {
  def weakETag = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Weak(tag)
  def strongETag = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Strong(tag)

  def etag = Gen.oneOf(weakETag, strongETag)

  def etags = HeadersSpec.headers(etag)
}

class ETagSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ETagSpec._

  describe("The ETag companion object") {
    it("should unapply on valid etags") {
      forAll(etag) { etag =>
        ETag.unapply(etag.toString) should be(Some(etag))
      }
    }

    it("should apply on valid etags") {
      forAll(etag) { etag =>
        ETag(etag.toString) should be(etag)
      }
    }
  }

  describe("An ETag instance") {
    it("should have the correct weak flag") {
      forAll(etag) {
        case etag @ ETag.Weak(_) => etag.isWeak should be(true)
        case etag @ ETag.Strong(_) => etag.isWeak should be(false)
      }
    }
  }
}
