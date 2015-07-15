package com.nrinaudo.fetch.grammar

import fastparse.Result.Success
import fastparse._
import org.scalacheck.Gen, Gen._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

object HttpGrammarSpec {
  val tokenChar: Gen[Char]  = oneOf(((32.toChar to 126.toChar).toSet &~ Separators).toSeq)
  // TODO: this is not entirely RFC compliant, as technically, ASCII control chars are supported.
  // This is currently ignored and known to break, but considered not to be worth the hassle.
  val quotableChar: Gen[Char] = oneOf(32.toChar to 126.toChar)

  def listBetween[T](min: Int, max: Int, gen: Gen[T]): Gen[List[T]] = for {
    size <- choose(min, max)
    ts   <- listOfN(size, gen)
  } yield ts

  val quotableString: Gen[String] = listOf(quotableChar).map(_.mkString)
  val tokenString: Gen[String] = nonEmptyListOf(tokenChar).map(_.mkString)
  val languageTagString: Gen[String] = listBetween(1, 8, alphaChar).map(_.mkString)
  val qValueFloat: Gen[Float] = choose(1, 1000).map(_ / 1000F)
}

class HttpGrammarSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HttpGrammarSpec._
  def parse[T](parser: Parser[T], str: String): Option[T] = parser.parse(str) match {
    case Success(t, _) => Some(t)
    case _             => None
  }

  describe("HttpGrammar") {
    it("should parse valid tokens") {
      forAll(tokenString) { text => parse(token, content(text)) should be(Some(text)) }
    }

    it("should parse valid quoted-strings") {
      forAll(quotableString) { text => parse(quotedString, quotedString(text)) should be(Some(text)) }
    }

    it("should parse valid parameters") {
      forAll(tokenString, quotableString) { (name, value) =>
        parse(param, param(name, value)) should be(Some(name -> value))
      }
    }

    it("should parse valid parameter lists") {
      forAll(Gen.mapOf(Gen.zip(tokenString, quotableString))) { ps =>
        parse(params, params(ps)) should be(Some(ps))
      }
    }

    // TODO: add parameters to media type parsing
    it("should parse */*") {
      parse(mediaType, "*/*") should be(Some(("*", "*", Map.empty)))
    }

    it("should parse valid media ranges") {
      forAll(tokenString) { range =>
        parse(mediaType, s"$range/*") should be(Some((range, "*", Map.empty)))
      }
    }

    it("should parse valid media types") {
      forAll(tokenString, tokenString) { (main, sub) =>
        parse(mediaType, s"$main/$sub") should be(Some((main, sub, Map.empty)))
      }
    }

    it("should parse valid language tags") {
      forAll(languageTagString) { (tag) =>
        parse(languageTag, tag) should be(Some(tag))
      }
    }

    it("should parse valid languages tags") {
      forAll(languageTagString, listOf(languageTagString)) { (main, subs) =>
        parse(language, language(main, subs)) should be(Some(main -> subs))
      }
    }

    it("should parse valid qvalues") {
      forAll(qValueFloat) { q =>
        parse(qValue, qValue(q)).orElse(fail(s"Failed to parse qValue $q")).foreach { parsed =>
          parsed should be((q * 1000).toInt / 1000F +- 0.002F)
        }
      }

      // Odd cases not generated by our serialization mechanism
      parse(qValue, "1.000") should be(Some(1))
      parse(qValue, "1.00") should be(Some(1))
      parse(qValue, "1.0") should be(Some(1))
    }

    it("should parse valid connegs") {
      forAll(tokenString, qValueFloat) { (value, q) =>
        parse(conneg(token), conneg(value, q)) should be(Some(value -> q))
      }
    }

    it("should parse valid conneg lists") {
      forAll(listOf(zip(tokenString, qValueFloat))) { cs =>
        parse(connegs(token), connegs(cs)) should be(Some(cs))
      }
    }
  }
}
