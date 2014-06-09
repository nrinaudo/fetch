package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object MediaTypeSpec {
  def main: Gen[String] = Gen.oneOf("text", "application", "video", "audio", "image", "message", "multipart")
  def sub: Gen[String] = Gen.oneOf("plain", "png", "jpg", "rdf", "html", "rdf+xml", "json", "x-fixed-field")

  def allType: Gen[MediaType.All] = Gen.const(MediaType.Everything)
  def range: Gen[MediaType.Range] = for(main <- main) yield MediaType.Range(main)
  def specific: Gen[MediaType.Specific] = for {
    main   <- main
    sub    <- sub
  } yield MediaType.Specific(main, sub)


  def mediaType: Gen[MediaType] = for {
    mediaType <- Gen.oneOf(allType, range, specific)
    params    <- MediaTypeParametersSpec.params
  } yield mediaType.params(params)

  def illegalMediaType: Gen[String] = Arbitrary.arbitrary[String].suchThat(_.indexOf('/') == -1)

  private def diffTypes[T](gen: Gen[T]) = for {
    t1 <- gen
    t2 <- gen if t1 != t2
  } yield (t1, t2)
}

class MediaTypeSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import MediaTypeSpec._
  import HttpGrammarSpec._
  import ConnegSpec._
  import Headers._

  def response(mediaType: MediaType) = Response(Status.Ok, new Headers().set("Content-Type", mediaType), "Test")

  describe("The MediaType companion object") {
    it("should unapply on responses with a content type") {
      forAll(mediaType) { mediaType =>
        MediaType.unapply(response(mediaType)) should be(Some(mediaType))
      }
    }

    it("should not unapply on responses without a content type") {
      MediaType.unapply(Response(Status.Ok, new Headers(), "Test")) should be(None)
    }
  }

  describe("A MediaType instance") {
    it("should serialize to itself") {
      forAll(mediaType) { mediaType => MediaType.parse(mediaType.toString) should be(Some(mediaType)) }
    }

    it("should have a working charset method") {
      forAll(mediaType, charset) { (mediaType, charset) =>
        mediaType.charset(charset).charset should be(Some(charset))
        mediaType.removeParam("charset").charset should be(None)
        mediaType.charset(charset).removeParam("charset").charset should be(None)
      }
    }

    it("should have a working param method") {
      forAll(mediaType, param) { case (mediaType, (name, value)) =>
        mediaType.param(name, value).param[String](name) should be(Some(value))
        mediaType.removeParam(name).param[String](name) should be(None)
        mediaType.param(name, value).removeParam(name).param[String](name) should be(None)
      }
    }
  }

  describe("A MediaType.Specific instance") {
    it("should unapply for other specific media type with the same raw type") {
      forAll(specific, params) { (mediaType, params) =>
        val full = mediaType.params(new MediaTypeParameters(params))
        mediaType.unapply(response(full)) should be(Some(response(full)))
        full.unapply(response(mediaType)) should be(Some(response(mediaType)))
      }
    }

    it("should not unapply on media ranges") {
      forAll(specific, range) { (mediaType, range) => mediaType.unapply(response(range)) should be(None) }
    }

    it("should not unapply on * / *") {
      forAll(specific, allType) { (mediaType, all) => mediaType.unapply(response(all)) should be(None) }
    }

    it("should not unapply for other specific media types with a different raw type") {
      forAll(diffTypes(specific)) { case (t1, t2) => t1.unapply(response(t2)) should be(None) }
    }
  }

  describe("A MediaType.Range instance") {
    it("should unapply for other media ranges with the same main type") {
      forAll(range, params) { (mediaType, params) =>
        val full = mediaType.params(new MediaTypeParameters(params))
        mediaType.unapply(response(full)) should be(Some(response(full)))
        full.unapply(response(mediaType)) should be(Some(response(mediaType)))
      }
    }

    it("should unapply on media types with the same main type") {
      forAll(range, sub) { (range, sub) =>
        val specific = range / sub
        range.unapply(response(specific)) should be(Some(response(specific)))
      }
    }

    it("should not unapply on media types with different main types") {
      forAll(diffTypes(range), sub) { case ((r1, r2), sub) => r1.unapply(response(r2 / sub)) should be(None) }
    }

    it("should not unapply on * / *") {
      forAll(range, allType) { (mediaType, all) => mediaType.unapply(response(all)) should be(None) }
    }

    it("should not unapply for other media ranges with a different raw type") {
      forAll(diffTypes(range)) { case (t1, t2) => t1.unapply(response(t2)) should be(None) }
    }
  }

  describe("A MediaType.All instance") {
    it("should unapply for any other instance of MediaType") {
      forAll(allType, mediaType) { (t1, t2) => t1.unapply(response(t2)) should be(Some(response(t2))) }
    }

    it("should not unapply for responses without a media type") {
      MediaType.Everything.unapply(new Response(Status.Ok, new Headers(), "Test")) should be(None)
    }
  }
}
