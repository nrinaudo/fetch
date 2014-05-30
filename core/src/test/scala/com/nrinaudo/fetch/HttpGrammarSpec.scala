package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object HttpGrammarSpec {
  val Separators = Set('(', ')', '<', '>', '@', ',', ';', ':', '\\', '"', '/', '[', ']', '?', '=', '{', '}', ' ', '\t')

  private val tokenChars = Gen.oneOf(((32.toChar to 126.toChar).toSet &~ Separators).toSeq)
  private val qdtextChars = Gen.oneOf(((0.toChar to 127.toChar).toSet - '\"' - '\\').toSeq)

  def char: Gen[Char] = Gen.oneOf(0.toChar, 127.toChar)

  private def string(gen: Gen[Char]): Gen[String] = for {
    chars <- Gen.nonEmptyListOf(gen)
  } yield chars.mkString

  def token: Gen[String] = string(tokenChars)
  def qdtext: Gen[String] = string(qdtextChars)
}

class HttpGrammarSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HttpGrammarSpec._

  describe("HttpGrammar") {
    it("should parse valid tokens") {
      forAll(token) { token => HttpGrammar.parseAll(HttpGrammar.token, token).get should be(token) }
    }

    it("should parse valid quoted pairs") {
      forAll(char) { c => HttpGrammar.parseAll(HttpGrammar.quotedPair, "\\" + c).get should be(c.toString) }
    }

    it("should parse valid qdtexts") {
      forAll(qdtext) { text => HttpGrammar.parseAll(HttpGrammar.qdtext, text).get should be(text) }
    }

    it("should parse valid quoted strings") {

    }
  }
}
