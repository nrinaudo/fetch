package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import java.nio.charset.Charset
import scala.collection.JavaConverters._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.nrinaudo.fetch.Conneg.MediaTypes

object ConnegSpec {
  private lazy val charsets = Charset.availableCharsets().values().asScala.toList

  def charset: Gen[Charset] = Gen.oneOf(charsets)

  def illegalCharset = Arbitrary.arbitrary[String].suchThat(!Charset.availableCharsets().containsKey(_))

  def conneg[T](gen: Gen[T]): Gen[Conneg[T]] = for {
    value <- gen
    q     <- Gen.choose(0, 1000)
  } yield Conneg(value, q / 1000f)

  def connegs[T](gen: Gen[T]): Gen[List[Conneg[T]]] = HeadersSpec.headers(conneg(gen))
}

class ConnegSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import ConnegSpec._
  import MediaTypeSpec._
  import EncodingSpec._
  import HeaderFormatSpec._
  import LanguageSpec._

  describe("Content negotiation headers") {
    it("should refuse illegal value of q") {
      forAll(Gen.oneOf(Gen.choose(1.1f, 100f), Gen.choose(-100f, -0.1f))) {q =>
        intercept[IllegalArgumentException] {Conneg("value", q)}
        ()
      }
    }

    it("should accept legal values of q") {
      forAll(Gen.choose(0f, 1f)) {q => Conneg("value", q); () }
    }

    it("should not serialize q when it's equal to 1") {
      forAll(charset) { charset => Conneg.Charsets.write(Seq(Conneg(charset, 1))) should be(Some(charset.name())) }
    }

    it("should assume an absent q defaults to 1.0") {
      forAll(charset) { charset =>
        Conneg.Charsets.read(charset.name()) should be(Some(List(Conneg(charset, 1.0f))))
      }
    }

    it("should correctly serialize and parse languages") {
      forAll(connegs(language)) { headers =>
        cycle(Conneg.Languages, headers) should be(Some(headers))
      }
    }

    it("should correctly serialize and parse charsets") {
      forAll(connegs(charset)) { headers =>
        cycle(Conneg.Charsets, headers) should be(Some(headers))
      }
    }

    it("should correctly serialize and parse media types") {
      forAll(connegs(mediaType)) { headers => cycle(MediaTypes, headers) should be(Some(headers)) }
    }

    it("should correctly serialize and parse content encodings") {
      forAll(connegs(encoding)) { headers =>
        cycle(Conneg.Encodings, headers) should be(Some(headers))
      }
    }
  }
}