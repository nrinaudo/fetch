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

    def diffTypes = for {
      t1 <- mediaType
      t2 <- mediaType if t1.rawType != t2.rawType
    } yield (t1, t2)

    it("should not unapply on responses with a different raw type") {
      forAll(diffTypes) { case (t1, t2) =>
          t1.unapply(response(t2)) should be(None)
       }
    }

    it("should unapply on responses with the same raw type") {
      forAll(mediaType) { mediaType =>
        val empty = mediaType.params(new MediaTypeParameters())

        empty.unapply(response(mediaType)) should be(Some(response(mediaType)))
        mediaType.unapply(response(empty)) should be(Some(response(empty)))
      }
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
}
