package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MediaTypeParametersSpec {
  // Note: q is not considered a valid media type parameter, as it would conflict with the Accept header's q parameter.
  def params: Gen[MediaTypeParameters] = HttpGrammarSpec.params.map(map => new MediaTypeParameters(map - "q"))
  def illegalParams = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)
}

class MediaTypeParametersSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MediaTypeParametersSpec._

  describe("MediaTypeParameters") {
    it("should not parse illegal values") {
      forAll(illegalParams) { params => MediaTypeParameters.parse(params).isDefined should be(false) }
    }

    it("should serialize to itself") {
      forAll(params) { params => MediaTypeParameters.parse(params.toString) should be(Some(params)) }
    }
  }
}
