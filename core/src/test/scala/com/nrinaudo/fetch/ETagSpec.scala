package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._

object ETagSpec {
  def weakTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Weak(tag)
  def strongTag: Gen[ETag] = for(tag <- Gen.identifier.suchThat(!_.isEmpty)) yield ETag.Strong(tag)

  implicit val etag: Arbitrary[ETag] = Arbitrary(Gen.oneOf(weakTag, strongTag))

  implicit val etags: Arbitrary[List[ETag]] = Arbitrary(HeadersSpec.headers(arbitrary[ETag]))

  def invalidEtag: Gen[String] = Arbitrary.arbitrary[String].suchThat { str =>
    str.length == 0 || str.charAt(0) != 'W' || str.charAt(0) != '\"' || str.charAt(str.length - 1) != '\"'
  }
}

class ETagSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  import ETagSpec._

  describe("The ETag companion object") {
    it("should parse valid etags") {
      forAll { etag: ETag => ETag.parse(etag.toString) should be(Some(etag)) }
    }

    it("should not parse invalid etags") {
      forAll(invalidEtag) { str => ETag.parse(str) should be(None) }
    }
  }

  describe("An ETag instance") {
    it("should have the correct weak flag") {
      forAll { etag: ETag =>
        etag match {
          case e@ETag.Weak(_) => e.isWeak should be(true)
          case e@ETag.Strong(_) => e.isWeak should be(false)
        }
      }
    }
  }
}