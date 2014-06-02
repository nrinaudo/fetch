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

    it("should serialize to itself") {
      forAll(status) { status =>
        Status(status.toString.toInt) should be(status)
      }
    }
  }
}
