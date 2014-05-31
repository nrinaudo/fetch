package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen

object HttpGrammarSpec {
  private val tokenChars = Gen.oneOf(((32.toChar to 126.toChar).toSet &~ HttpGrammar.Separators).toSeq)
  private val qdtextChars = Gen.oneOf(((0.toChar to 127.toChar).toSet - '\"' - '\\').toSeq)

  def char: Gen[Char] = Gen.choose(0.toChar, 127.toChar)

  private def stringOf(gen: Gen[Char]): Gen[String] = for {
    chars <- Gen.nonEmptyListOf(gen)
  } yield chars.mkString

  def token: Gen[String]   = stringOf(tokenChars)
  def qdtext: Gen[String]  = stringOf(qdtextChars)
  def content: Gen[String] = Gen.nonEmptyListOf(char).map(_.mkString)

  def param: Gen[(String, String)] = for {
    name  <- token
    value <- content
  } yield (name, value)

  def params: Gen[Map[String, String]] = for {
    n    <- Gen.choose(0, 10)
    list <- Gen.listOfN(n, param)
  } yield list.foldLeft(Map[String, String]()) { case (params, param) => params + param }
}

class HttpGrammarSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks with HttpGrammar {
  import HttpGrammarSpec._

  describe("HttpGrammar") {
    it("should parse valid tokens") {
      forAll(HttpGrammarSpec.token) { text => parseAll(token, text).get should be(text) }
    }

    it("should parse valid quoted pairs") {
      forAll(char) { c => parseAll(quotedPair, "\\" + c).get should be(c.toString) }
    }

    it("should parse valid qdtexts") {
      forAll(HttpGrammarSpec.qdtext) { text => parseAll(qdtext, text).get should be(text) }
    }

    it("should parse valid values") {
      forAll(HttpGrammarSpec.content) { string => parseAll(content, content(string)).get should be(string) }
    }

    // TODO: test error cases
    // TODO: test param parsers.
  }
}
