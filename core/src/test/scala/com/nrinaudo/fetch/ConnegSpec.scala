package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import java.nio.charset.Charset
import scala.collection.JavaConverters._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import com.nrinaudo.fetch.Conneg.MediaTypes

object ConnegSpec {
  private lazy val charsets: List[Charset] = Charset.availableCharsets().values().asScala.toList

  implicit val arbCharset: Arbitrary[Charset] = Arbitrary(Gen.oneOf(charsets))

  def illegalCharset: Gen[String] = Arbitrary.arbitrary[String].suchThat(!Charset.availableCharsets().containsKey(_))

  implicit def conneg[A: Arbitrary]: Arbitrary[Conneg[A]] = Arbitrary {
    for {
      value <- arbitrary[A]
      q     <- Gen.choose(0, 1000)
    } yield Conneg(value, q / 1000f)
  }

  implicit def connegs[A: Arbitrary]: Arbitrary[List[Conneg[A]]] = Arbitrary(HeadersSpec.headers(arbitrary[Conneg[A]]))
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
      forAll { charset: Charset => Conneg.Charsets.write(Seq(Conneg(charset, 1))) should be(Some(charset.name())) }
    }

    it("should assume an absent q defaults to 1.0") {
      forAll { charset: Charset =>
        Conneg.Charsets.read(charset.name()) should be(Some(List(Conneg(charset, 1.0f))))
      }
    }

    it("should correctly serialize and parse languages") {
      forAll { headers: List[Conneg[Language]] =>
        cycle(Conneg.Languages, headers) should be(Some(headers))
      }
    }

    it("should correctly serialize and parse charsets") {
      forAll { headers: List[Conneg[Charset]] =>
        cycle(Conneg.Charsets, headers) should be(Some(headers))
      }
    }

    it("should correctly serialize and parse media types") {
      forAll { headers: List[Conneg[MediaType]] => cycle(MediaTypes, headers) should be(Some(headers)) }
    }

    it("should correctly serialize and parse content encodings") {
      forAll { headers: List[Conneg[Encoding]] =>
        cycle(Conneg.Encodings, headers) should be(Some(headers))
      }
    }
  }
}