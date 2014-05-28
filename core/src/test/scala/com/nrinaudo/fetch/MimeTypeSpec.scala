package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MimeTypeSpec {
  def main: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def sub: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  def mimeType: Gen[MimeType] = for {
    main   <- main
    sub    <- sub
    params <- MimeTypeParametersSpec.params
  } yield MimeType(main, sub, params)

  def illegalMimeType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)
}

class MimeTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MimeTypeSpec._

  describe("A MIME type") {
    it("should serialize to itself") {
      forAll(mimeType) { mime => MimeType(mime.toString) should be(mime) }
    }
  }
}
