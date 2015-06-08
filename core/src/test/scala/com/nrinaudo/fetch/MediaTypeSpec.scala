package com.nrinaudo.fetch

import java.nio.charset.Charset

import com.nrinaudo.fetch.Generators._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class MediaTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import Headers._

  def diffTypes[T](gen: Gen[T]): Gen[(T, T)] = for {
    t1 <- gen
    t2 <- gen if t1 != t2
  } yield (t1, t2)

  def response(mediaType: MediaType) = Response(Status.Ok, Parameters.empty.set("Content-Type", mediaType), "Test")

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
      forAll(arbitrary[MediaType.Specific], arbitrary[Parameters]) { (mediaType, params) =>
        val full = mediaType.params(params)
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
      forAll(arbitrary[MediaType.Range], arbitrary[Parameters]) { (mediaType, params) =>
        val full = mediaType.params(params)
        validateUnapply(mediaType, full, true)
        validateUnapply(full, mediaType, true)
      }
    }

    it("should unapply on media types with the same main type") {
      forAll(arbitrary[MediaType.Range], subType) { (range, sub) => validateUnapply(range, range / sub, true) }
    }

    it("should not unapply on media types with different main types") {
      forAll(diffTypes(arbitrary[MediaType.Range]), subType) { case ((r1, r2), sub) => validateUnapply(r1, r2 / sub, false) }
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
      MediaType.Everything.unapply(new Response(Status.Ok, Parameters.empty, "Test")) should be(None)
    }
  }
}
