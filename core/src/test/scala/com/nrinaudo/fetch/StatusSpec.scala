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
}

class StatusSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import StatusSpec._

  describe("Status") {
    it("should unapply legal statuses correctly") {
      forAll(status) { status =>
        Status.unapply(status.code) should be(Some(status))
      }
    }

    it("should refuse to unapply illegal statuses") {
      forAll(Arbitrary.arbitrary[Int].suchThat(i => i < 0 || i > 600)) { status =>
        Status.unapply(status) should be(None)
      }
    }

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
  }
}
