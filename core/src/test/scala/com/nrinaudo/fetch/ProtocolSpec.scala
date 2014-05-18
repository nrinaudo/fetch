package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object ProtocolSpec {
  /** Generates a random supported protocol (http or https). */
  def protocol = Gen.oneOf(Protocol.Http, Protocol.Https)
}

class ProtocolSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._

  describe("Protocol") {
    it("should apply / unapply valid instances correctly") {
      forAll(protocol) { protocol =>
        Protocol.unapply(protocol.name) should be(Some(protocol))
        Protocol(protocol.name) should be(protocol)
      }
    }

    it("should apply / unapply invalid instances correctly") {
      forAll(Arbitrary.arbitrary[String].suchThat(s => s != Protocol.Http.name && s != Protocol.Https.name)) { str =>
        Protocol.unapply(str) should be(None)
        intercept[IllegalArgumentException] {Protocol(str)}
      }
    }

    it("should generate URLs correctly") {
      forAll(protocol, UrlSpec.host) { (protocol, host) =>
        (protocol :/ host).host should be(host)
        protocol.host(host).host should be(host)
      }
    }
  }
}