package com.nrinaudo.fetch

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object MimeTypeSpec {
  def main = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def sub = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")
  def param = for {
    name  <- Gen.identifier
    value <- Gen.identifier
  } yield (name, value)

  def params = for(list <- Gen.listOf(param)) yield
    list.foldLeft(Map[String, String]()) {case (map, (name, value)) => map + (name -> value)}

  def mimeType = for {
    main   <- main
    sub    <- sub
    params <- params
  } yield MimeType(main, sub, params)
}

class MimeTypeSpec extends FunSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {
  import MimeTypeSpec._

  describe("A MIME type") {
    it("should parse legal types without parameters") {
      forAll(main, sub) {(main, sub) =>
        MimeType(MimeType(main, sub).toString) should be(MimeType(main, sub))
      }
    }

    it("should parse legal types with parameters") {
      forAll(main, sub, params) { (main, sub, params) =>
        MimeType(MimeType(main, sub, params).toString) should be(MimeType(main, sub, params))
      }
    }
  }
}
