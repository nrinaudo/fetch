package com.nrinaudo.fetch

import org.scalacheck.Gen
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object ProtocolSpec {
  /** Generates a random supported protocol (http or https). */
  def protocol = Gen.oneOf(Protocol.Http, Protocol.Https)
}

class ProtocolSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ProtocolSpec._

  describe("Protocol") {
    it("should parse valid instances correctly") {
      forAll(protocol) { protocol =>
        Protocol.unapply(protocol.name) should be(Some(protocol))
      }
    }
  }
}