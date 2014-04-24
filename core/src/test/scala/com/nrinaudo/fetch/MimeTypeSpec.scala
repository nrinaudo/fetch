package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
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

class MimeTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MimeTypeSpec._

  describe("A MIME type") {
    it("should serialize to itself") {
      forAll(mimeType) { mime =>
        val MimeType(parsed) = mime.toString

        parsed should be(mime)
      }
    }
  }
}
