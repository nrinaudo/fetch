package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Gen, Arbitrary}

object ETagSpec {
  def etag = for {
    weak  <- Arbitrary.arbitrary[Boolean]
    value <- Gen.identifier.suchThat(!_.isEmpty)
  } yield
    if(weak) WeakTag(value)
    else     StrongTag(value)

}

class ETagSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ETagSpec._

  describe("An ETag") {
    it("should parse valid instances correctly") {
      forAll(etag) { etag =>
        ETag(etag.toString) should be(etag)
      }
    }
  }
}
