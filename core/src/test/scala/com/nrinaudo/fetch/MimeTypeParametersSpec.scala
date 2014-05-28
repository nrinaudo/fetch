package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MimeTypeParametersSpec {
  def param: Gen[(String, String)] = for {
      name  <- Gen.identifier
      value <- Gen.identifier
    } yield (name, value)

  def params: Gen[MimeTypeParameters] = for {
    n    <- Gen.choose(0, 10)
    list <- Gen.listOfN(n, param)
  } yield list.foldLeft(new MimeTypeParameters()) {case (params, (name, value)) => params.set(name, value)}

    def illegalParams = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)
}

class MimeTypeParametersSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MimeTypeParametersSpec._

  describe("MimeTypeParameters") {
    it("should not unapply on illegal values") {
      forAll(illegalParams) { params => MimeTypeParameters.unapply(params).isDefined should be(false) }
    }

    it("should fail to apply on illegal values") {
      forAll(illegalParams) { params => intercept[IllegalArgumentException] {MimeTypeParameters(params)} }
    }

    it("should serialize to itself") {
      forAll(params) { params => MimeTypeParameters(params.toString) should be(params) }
    }
  }
}
