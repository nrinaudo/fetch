package com.nrinaudo.fetch

import org.scalacheck.Gen

object ProtocolSpec {
  /** Generates a random supported protocol (http or https). */
  def protocol = Gen.oneOf(Protocol.Http, Protocol.Https)
}

// TODO: add serialization / deserialization tests