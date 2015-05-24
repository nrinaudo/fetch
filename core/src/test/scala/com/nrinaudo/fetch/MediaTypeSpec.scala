package com.nrinaudo.fetch

import java.nio.charset.Charset

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._

object MediaTypeSpec {
  def main: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def sub: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  implicit val allType: Arbitrary[MediaType.All] = Arbitrary(Gen.const(MediaType.Everything))
  implicit val range: Arbitrary[MediaType.Range] = Arbitrary(for(main <- main) yield MediaType.Range(main))
  implicit val specific: Arbitrary[MediaType.Specific] = Arbitrary {
    for {
      main   <- main
      sub    <- sub
    } yield MediaType.Specific(main, sub)
  }

  implicit val mediaType: Arbitrary[MediaType] = Arbitrary {
    for {
      mediaType <- Gen.oneOf(arbitrary[MediaType.All], arbitrary[MediaType.Range], arbitrary[MediaType.Specific])
      params    <- MediaTypeParametersSpec.params
    } yield mediaType.params(params)
  }

  def illegalMediaType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)

  private def diffTypes[T](gen: Gen[T]): Gen[(T, T)] = for {
    t1 <- gen
    t2 <- gen if t1 != t2
  } yield (t1, t2)
}

class MediaTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MediaTypeSpec._
  import HttpGrammarSpec._
  import ConnegSpec._
  import Headers._

  def response(mediaType: MediaType) = Response(Status.Ok, Headers.empty.set("Content-Type", mediaType), "Test")

  describe("A MediaType instance") {
    it("should serialize to itself") {
      forAll { mediaType: MediaType => MediaType.parse(mediaType.toString) should be(Some(mediaType)) }
    }

    it("should have a working charset method") {
      forAll { (mediaType: MediaType, charset: Charset) =>
        mediaType.charset(charset).charset should be(Some(charset))
        mediaType.removeParam("charset").charset should be(None)
        mediaType.charset(charset).removeParam("charset").charset should be(None)
      }
    }

    it("should have a working param method") {
      forAll { (mediaType: MediaType, p: Param) =>
        mediaType.param(p.name, p.value).param[String](p.name) should be(Some(p.value))
        mediaType.removeParam(p.name).param[String](p.name) should be(None)
        mediaType.param(p.name, p.value).removeParam(p.name).param[String](p.name) should be(None)
      }
    }
  }

  def validateUnapply(matcher: MediaType, matched: MediaType, expected: Boolean): Unit = {
    if(expected) {
      matcher.unapply(matched) should be(Some(matched))
      matcher.unapply(response(matched)).isDefined should be(true)
    }
    else {
      matcher.unapply(matched) should be(None)
      matcher.unapply(response(matched)).isDefined should be(false)
    }
  }

  describe("A MediaType.Specific instance") {
    it("should unapply for other specific media type with the same raw type") {
      forAll(arbitrary[MediaType.Specific], params) { (mediaType, params) =>
        val full = mediaType.params(new MediaTypeParameters(params))
        validateUnapply(mediaType, full, true)
        validateUnapply(full, mediaType, true)
      }
    }

    it("should not unapply on media ranges") {
      forAll { (mediaType: MediaType.Specific, range: MediaType.Range) => validateUnapply(mediaType, range, false) }
    }

    it("should not unapply on */*") {
      forAll { (mediaType: MediaType.Specific, all: MediaType.All) => validateUnapply(mediaType, all, false) }
    }

    it("should not unapply for other specific media types with a different raw type") {
      forAll(diffTypes(arbitrary[MediaType.Specific])) { case (t1, t2) => validateUnapply(t1, t2, false) }
    }
  }

  describe("A MediaType.Range instance") {
    it("should unapply for other media ranges with the same main type") {
      forAll(arbitrary[MediaType.Range], params) { (mediaType, params) =>
        val full = mediaType.params(new MediaTypeParameters(params))
        validateUnapply(mediaType, full, true)
        validateUnapply(full, mediaType, true)
      }
    }

    it("should unapply on media types with the same main type") {
      forAll(arbitrary[MediaType.Range], sub) { (range, sub) => validateUnapply(range, range / sub, true) }
    }

    it("should not unapply on media types with different main types") {
      forAll(diffTypes(arbitrary[MediaType.Range]), sub) { case ((r1, r2), sub) => validateUnapply(r1, r2 / sub, false) }
    }

    it("should not unapply on */*") {
      forAll { (mediaType: MediaType.Range, all: MediaType.All) => validateUnapply(mediaType, all, false) }
    }

    it("should not unapply for other media ranges with a different raw type") {
      forAll(diffTypes(arbitrary[MediaType.Range])) { case (t1, t2) => validateUnapply(t1, t2, false) }
    }
  }

  describe("A MediaType.All instance") {
    it("should unapply for any other instance of MediaType") {
      forAll { (t1: MediaType.All, t2: MediaType) => validateUnapply(t1, t2, true) }
    }

    it("should not unapply for responses without a media type") {
      MediaType.Everything.unapply(new Response(Status.Ok, Headers.empty, "Test")) should be(None)
    }
  }
}
