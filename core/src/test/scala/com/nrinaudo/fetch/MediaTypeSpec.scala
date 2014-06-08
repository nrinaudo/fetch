package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MediaTypeSpec {
  def main: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def sub: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  def allType: Gen[MediaType] = Gen.const(MediaType.Everything)
  def range: Gen[MediaType] = for(main <- main) yield MediaType.Range(main)
  def specific: Gen[MediaType] = for {
      main   <- main
      sub    <- sub
    } yield MediaType.Specific(main, sub)


  def mediaType: Gen[MediaType] = for {
    mediaType <- Gen.oneOf(allType, range, specific)
    params    <- MediaTypeParametersSpec.params
  } yield mediaType.params(params)

  def illegalMediaType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)
}

class MediaTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MediaTypeSpec._

  describe("A media type") {
    it("should serialize to itself") {
      forAll(mediaType) { mediaType => MediaType.parse(mediaType.toString) should be(Some(mediaType)) }
    }
  }
}
