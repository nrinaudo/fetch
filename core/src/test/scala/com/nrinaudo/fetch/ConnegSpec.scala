package com.nrinaudo.fetch

import org.scalacheck.Gen
import java.nio.charset.Charset
import java.util.Locale
import scala.collection.JavaConverters._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scala.util.Success

object ConnegSpec {
  private lazy val charsets = Charset.availableCharsets().values().asScala.toList

  def charset: Gen[Charset] = Gen.oneOf(charsets)

  def language: Gen[Locale] = Gen.oneOf(Locale.ENGLISH, Locale.FRENCH, Locale.GERMAN, Locale.ITALIAN, Locale.JAPANESE,
    Locale.KOREAN, Locale.CHINESE, Locale.SIMPLIFIED_CHINESE, Locale.TRADITIONAL_CHINESE, Locale.FRANCE, Locale.GERMANY,
    Locale.ITALY, Locale.JAPAN, Locale.KOREA, Locale.CHINA, Locale.PRC, Locale.TAIWAN, Locale.UK, Locale.US,
    Locale.CANADA, Locale.CANADA_FRENCH)


  def conneg[T](gen: Gen[T]): Gen[Conneg[T]] = for {
    value <- gen
    q     <- Gen.choose(0, 1000)
  } yield Conneg(value, q / 1000f)

  def connegs[T](gen: Gen[T]): Gen[List[Conneg[T]]] = HeaderSpec.headers(conneg(gen))
}

class ConnegSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ConnegSpec._
  import MimeTypeSpec._
  import EncodingSpec._
  import HeaderFormatSpec._

  describe("Content negotiation headers") {
    it("should refuse illegal value of q") {
      forAll(Gen.oneOf(Gen.choose(1.1f, 100f), Gen.choose(-100f, -0.1f))) {q =>
        intercept[IllegalArgumentException] {Conneg("value", q)}
      }
    }

    it("should accept legal values of q") {
      forAll(Gen.choose(0f, 1f)) {q =>
        Conneg("value", q)
      }
    }

    it("should not serialize q when it's equal to 1") {
      Conneg.ConnegCharset.write(Conneg(Charset.forName("UTF-8"), 1)) should be(Some("UTF-8"))
    }

    it("should assume an absent q defaults to 1.0") {
      Conneg.ConnegCharset.read("UTF-8") should be(Success(Conneg(Charset.forName("UTF-8"), 1.0f)))
    }

    it("should correctly serialize and parse languages") {
      forAll(connegs(language)) { headers =>
        cycle(Headers.compositeFormat[Conneg[Locale]], headers) should be(Success(headers))
      }
    }

    it("should correctly serialize and parse charsets") {
      forAll(connegs(charset)) { headers =>
        cycle(Headers.compositeFormat[Conneg[Charset]], headers) should be(Success(headers))
      }
    }

    it("should correctly serialize and parse MIME types") {
      forAll(connegs(mimeType)) { headers =>
      // TODO: we're not perfectly RFC compliant when it comes to parsing Accept: parameters break the parser.
        val fixed = headers.map(_.map(_.copy(params = Map())))
        cycle(Headers.compositeFormat[Conneg[MimeType]], fixed) should be(Success(fixed))
      }
    }

    it("should correctly serialize and parse content encodings") {
      forAll(connegs(encoding)) { headers =>
        cycle(Headers.compositeFormat[Conneg[Encoding]], headers) should be(Success(headers))
      }
    }
  }
}