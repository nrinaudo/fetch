package com.nrinaudo.fetch

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

object MediaTypeParametersSpec {
  case class Param(name: String, value: String)

  // Note: q is not considered a valid media type parameter, as it would conflict with the Accept header's q parameter.
  implicit val arbParam: Arbitrary[Param] = Arbitrary {
    for {
      name <-  identifier if name != "q"
      value <- nonEmptyListOf(oneOf(32.toChar to 126.toChar)).map(_.mkString)
    } yield Param(name, value)
  }

  // Note: we don't generate more than 5 parameters because this causes unrelated issues when testing - some HTTP
  // servers seem to have a limit to the size of a header value.
  def params: Gen[Parameters] = for {
    size <- choose(0, 5)
    ps   <- mapOfN(size, arbitrary[Param].map(p => p.name -> p.value))
  } yield Parameters(ps)

  def illegalParams: Gen[String] = Arbitrary.arbitrary[String].suchThat(str => !str.contains('=') && !str.isEmpty)
}

class MediaTypeParametersSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MediaTypeParametersSpec._

  describe("MediaTypeParameters") {
    it("should not parse illegal values") {
      forAll(illegalParams) { params => MediaTypeParameters.parse(params).isDefined should be(false) }
    }

    it("should serialize to itself") {
      forAll(params) { params => MediaTypeParameters.parse(grammar.params(params.values)) should be(Some(params)) }
    }
  }
}
