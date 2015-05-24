package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._

object HttpGrammarSpec {
  def tokenChar: Gen[Char]  = Gen.oneOf(((32.toChar to 126.toChar).toSet &~ HttpGrammar.Separators).toSeq)
  def qdtextChar: Gen[Char] = Gen.oneOf(((0.toChar to 127.toChar).toSet - '\"' - '\\').toSeq)
  // TODO: this is not entirely RFC compliant, as technically, ASCII control chars are supported.
  // This is currently ignored and known to break, but considered not to be worth the hassle.
  def char: Gen[Char]       = Gen.choose(32.toChar, 126.toChar)

  private def stringOf(gen: Gen[Char]): Gen[String] = for {
    chars <- Gen.nonEmptyListOf(gen)
  } yield chars.mkString

  def token: Gen[String]   = stringOf(tokenChar)
  def qdtext: Gen[String]  = stringOf(qdtextChar)
  def content: Gen[String] = stringOf(char)

  case class Param(name: String, value: String)
  implicit val param: Arbitrary[Param] = Arbitrary {
    for {
      name <- token
      value <- content
    } yield Param(name, value)
  }

  def params: Gen[Map[String, String]] = for {
    n    <- Gen.choose(0, 10)
    list <- Gen.listOfN(n, arbitrary[Param])
  } yield list.foldLeft(Map[String, String]()) { case (params, p) => params + (p.name -> p.value) }
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
