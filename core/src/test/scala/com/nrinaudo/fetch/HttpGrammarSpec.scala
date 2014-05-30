package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object HttpGrammarSpec {
  val Separators = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{', '}', ' ', '\t')

  private val tokenChars = Gen.oneOf(((32.toChar to 126.toChar).toSet &~ Separators).toSeq)

  def token: Gen[String] = for {
    chars <- Gen.nonEmptyListOf(tokenChars)
  } yield chars.mkString
}

class HttpGrammarSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HttpGrammarSpec._

  describe("HttpGrammar") {
    it("should parse valid tokens") {
      forAll(token) { token => HttpGrammar.parseAll(HttpGrammar.token, token).get should be(token) }
    }
  }
}
