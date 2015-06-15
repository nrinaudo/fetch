package com.nrinaudo.fetch

import java.nio.charset.Charset

import com.nrinaudo.fetch.Conneg._
import com.nrinaudo.fetch.Generators._
import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

class ConnegFormatSpec[T](override val illegalT: Gen[String])(implicit arb: Arbitrary[T],
                                                              writerT: ValueWriter[T],
                                                              override val reader: ValueReader[Seq[Conneg[T]]],
                                                              override val writer: ValueWriter[Seq[Conneg[T]]]) extends ValueFormatSpec.WithErrors[Seq[Conneg[T]]] {
  override implicit def arbT = connegs[T]

  it("should refuse illegal value of q") {
    forAll(arbitrary[T], Gen.oneOf(choose(1.1f, 100f), choose(-100f, -0.1f))) { (t, q) =>
      intercept[IllegalArgumentException] {Conneg(t, q)}
      ()
    }
  }

  it("should accept legal values of q") {
    forAll(arbitrary[T], choose(0f, 1f)) { (t, q) => Conneg(t, q); () }
  }


  it("should not serialize q when it's equal to 1") {
    forAll { t: T => writer.write(Seq(Conneg(t, 1))) should be(writerT.write(t)) }
  }

  it("should assume an absent q defaults to 1.0") {
    forAll { t: T =>
      reader.read(writerT.write(t).get) should be(Some(List(Conneg(t, 1.0f))))
    }
  }
}

class CharsetConnegFormatSpec extends ConnegFormatSpec[Charset](illegalCharsets)
class LanguageConnegFormatSpec extends ConnegFormatSpec[Language](illegalLanguages)
class EncodingConnegFormatSpec extends ConnegFormatSpec[Encoding](illegalEncodings)
class MediaTypeConnegFormatSpec extends ConnegFormatSpec[MediaType](illegalMediaTypes)
