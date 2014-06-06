package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}

object StatusSpec {
  def success     = for(status <- Gen.choose(200, 299)) yield Status(status)
  def redirection = for(status <- Gen.choose(300, 399)) yield Status(status)
  def clientError = for(status <- Gen.choose(400, 499)) yield Status(status)
  def serverError = for(status <- Gen.choose(500, 599)) yield Status(status)

  def status = Gen.oneOf(success, redirection, clientError, serverError)

  def invalidStatus: Gen[Int] = Arbitrary.arbitrary[Int].suchThat(i => i < 0 || i > 600)
  private def response(status: Status) = new Response(status, new Headers(), status.code)
}

class StatusSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import StatusSpec._


  describe("The Status companion object") {
    it("should apply on legal statuses") {
      forAll(status) { status => Status(status.code) should be(status) }
    }

    it("should fail to apply on illegal statuses") {
      forAll(invalidStatus) { status => intercept[IllegalArgumentException](Status(status)) }
    }

    def expected[T](t: T, pass: Boolean) =
      if(pass) Some(t)
      else None

    def validate(status: Status, extractor: Status.Extractor, pass: Boolean) {
      extractor.unapply(status) should be(expected(status, pass))
      extractor.unapply(response(status)) should be(expected(response(status), pass))
    }

    it("should extract success statuses") {
      forAll(success) { status =>
        validate(status, Status.Success, true)
        validate(status, Status.Redirection, false)
        validate(status, Status.ClientError, false)
        validate(status, Status.ServerError, false)
        validate(status, Status.Error, false)
      }
    }

    it("should extract redirection statuses") {
      forAll(redirection) { status =>
        validate(status, Status.Success, false)
        validate(status, Status.Redirection, true)
        validate(status, Status.ClientError, false)
        validate(status, Status.ServerError, false)
        validate(status, Status.Error, false)
      }
    }

    it("should extract client error statuses") {
      forAll(clientError) { status =>
        validate(status, Status.Success, false)
        validate(status, Status.Redirection, false)
        validate(status, Status.ClientError, true)
        validate(status, Status.ServerError, false)
        validate(status, Status.Error, true)
      }
    }

    it("should extract server error statuses") {
      forAll(serverError) { status =>
        validate(status, Status.Success, false)
        validate(status, Status.Redirection, false)
        validate(status, Status.ClientError, false)
        validate(status, Status.ServerError, true)
        validate(status, Status.Error, true)
      }
    }
  }

  describe("An instance of Status") {
    it("should detect success statuses correctly") {
      forAll(success) { status =>
        status.isSuccess should be(true)
        status.isRedirection should be(false)
        status.isClientError should be(false)
        status.isServerError should be(false)
      }
    }

    it("should detect redirection statuses correctly") {
      forAll(redirection) { status =>
        status.isSuccess should be(false)
        status.isRedirection should be(true)
        status.isClientError should be(false)
        status.isServerError should be(false)
      }
    }

    it("should detect client errors correctly") {
      forAll(clientError) { status =>
        status.isSuccess should be(false)
        status.isRedirection should be(false)
        status.isClientError should be(true)
        status.isServerError should be(false)
      }
    }

    it("should detect server errors correctly") {
      forAll(serverError) { status =>
        status.isSuccess should be(false)
        status.isRedirection should be(false)
        status.isClientError should be(false)
        status.isServerError should be(true)
      }
    }

    it("should unapply responses with the same status") {
      forAll(status) { status => status.unapply(response(status)) should be (Some(response(status))) }
    }

    def diffStatuses = for {
      s1 <- status
      s2 <- status if s1 != s2
    } yield (s1, s2)

    it("should not unapply responses with a different status") {
      forAll(diffStatuses) { case (s1, s2) =>
        s1.unapply(response(s2)) should be(None)
      }
    }

    it("should serialize to itself") {
      forAll(status) { status =>
        Status(status.toString.toInt) should be(status)
      }
    }
  }
}
