package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary.arbitrary

object StatusSpec {
  val success: Gen[Status]     = for(status <- Gen.choose(200, 299)) yield Status(status)
  val redirection: Gen[Status] = for(status <- Gen.choose(300, 399)) yield Status(status)
  val clientError: Gen[Status] = for(status <- Gen.choose(400, 499)) yield Status(status)
  val serverError: Gen[Status] = for(status <- Gen.choose(500, 599)) yield Status(status)

  implicit val status: Arbitrary[Status] = Arbitrary(Gen.oneOf(success, redirection, clientError, serverError))

  def invalidStatus: Gen[Int] = Arbitrary.arbitrary[Int].suchThat(i => i < 0 || i > 600)
  private def response(status: Status) = new Response(status, Headers.empty, status.code)
}

class StatusSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import StatusSpec._


  describe("The Status companion object") {
    it("should apply on legal statuses") {
      forAll { status: Status => Status(status.code) should be(status) }
    }

    it("should fail to apply on illegal statuses") {
      forAll(invalidStatus) { status => intercept[IllegalArgumentException](Status(status)); () }
    }

    def expected[T](t: T, pass: Boolean) =
      if(pass) Some(t)
      else None

    def validateUnapply(status: Status, extractor: Status.Extractor, pass: Boolean): Unit = {
      extractor.unapply(status) should be(expected(status, pass))
      extractor.unapply(response(status)).isDefined should be(pass)
    }

    it("should extract success statuses") {
      forAll(success) { status =>
        validateUnapply(status, Status.Success, true)
        validateUnapply(status, Status.Redirection, false)
        validateUnapply(status, Status.ClientError, false)
        validateUnapply(status, Status.ServerError, false)
        validateUnapply(status, Status.Error, false)
      }
    }

    it("should extract redirection statuses") {
      forAll(redirection) { status =>
        validateUnapply(status, Status.Success, false)
        validateUnapply(status, Status.Redirection, true)
        validateUnapply(status, Status.ClientError, false)
        validateUnapply(status, Status.ServerError, false)
        validateUnapply(status, Status.Error, false)
      }
    }

    it("should extract client error statuses") {
      forAll(clientError) { status =>
        validateUnapply(status, Status.Success, false)
        validateUnapply(status, Status.Redirection, false)
        validateUnapply(status, Status.ClientError, true)
        validateUnapply(status, Status.ServerError, false)
        validateUnapply(status, Status.Error, true)
      }
    }

    it("should extract server error statuses") {
      forAll(serverError) { status =>
        validateUnapply(status, Status.Success, false)
        validateUnapply(status, Status.Redirection, false)
        validateUnapply(status, Status.ClientError, false)
        validateUnapply(status, Status.ServerError, true)
        validateUnapply(status, Status.Error, true)
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
      forAll { status: Status => status.unapply(response(status)).isDefined should be(true) }
    }

    def diffStatuses = for {
      s1 <- arbitrary[Status]
      s2 <- arbitrary[Status] if s1 != s2
    } yield (s1, s2)

    it("should not unapply responses with a different status") {
      forAll(diffStatuses) { case (s1, s2) =>
        s1.unapply(response(s2)) should be(None)
      }
    }

    it("should serialize to itself") {
      forAll { status: Status =>
        Status(status.toString.toInt) should be(status)
      }
    }
  }
}
