package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MimeTypeParametersSpec {
  // Note: q is not considered a valid MIME type parameter, as it would conflict with the Accept header's q parameter.
  def params: Gen[MimeTypeParameters] = HttpGrammarSpec.params.map(map => new MimeTypeParameters(map - "q"))
  def illegalParams = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)
}

class MimeTypeParametersSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MimeTypeParametersSpec._

  describe("MimeTypeParameters") {
    it("should not parse illegal values") {
      forAll(illegalParams) { params => MimeTypeParameters.parse(params).isDefined should be(false) }
    }

    it("should serialize to itself") {
      forAll(params) { params => MimeTypeParameters.parse(params.toString) should be(Some(params)) }
    }
  }
}
