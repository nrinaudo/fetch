package com.nrinaudo.fetch

import java.nio.charset.Charset

import com.nrinaudo.fetch.Conneg._
import com.nrinaudo.fetch.Generators._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

class ConnegSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import HeaderFormatSpec._

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
      forAll { charset: Charset => Charsets.write(Seq(Conneg(charset, 1))) should be(Some(charset.name())) }
    }

    it("should assume an absent q defaults to 1.0") {
      forAll { charset: Charset =>
        Charsets.read(charset.name()) should be(Some(List(Conneg(charset, 1.0f))))
      }
    }

    it("should correctly serialize and parse languages") {
      forAll { headers: Seq[Conneg[Language]] => cycle(headers) should be(Some(headers)) }
    }

    it("should correctly serialize and parse charsets") {
      forAll { headers: Seq[Conneg[Charset]] => cycle(headers) should be(Some(headers)) }
    }

    it("should correctly serialize and parse media types") {
      forAll { headers: Seq[Conneg[MediaType]] => cycle(headers) should be(Some(headers)) }
    }

    it("should correctly serialize and parse content encodings") {
      forAll { headers: Seq[Conneg[Encoding]] =>
        cycle(headers) should be(Some(headers))
      }
    }
  }
}