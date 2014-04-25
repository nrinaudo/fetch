package com.nrinaudo.fetch

import org.scalacheck.Gen
import java.nio.charset.Charset
import java.util.Locale
import org.scalacheck.Arbitrary._
import scala.collection.JavaConverters._
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

object ConnegSpec {
  private lazy val charsets = Charset.availableCharsets().values().asScala.toList

  def charset: Gen[Charset] = Gen.oneOf(charsets)

  def language: Gen[Locale] = Gen.oneOf(Locale.getAvailableLocales)


  def conneg[T](gen: Gen[T]): Gen[Conneg[T]] = for {
    value <- gen
    q     <- arbitrary[Float]
  } yield Conneg(value, math.abs(q / Float.MaxValue))

  def connegs[T](gen: Gen[T]): Gen[List[Conneg[T]]] = for {
    l    <- Gen.choose(1, 10)
    list <- Gen.listOfN(l, conneg(gen))
  } yield list
}

class ConnegSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  describe("A content negotiation header") {
    it("should refuse illegal q values") {
      forAll(Gen.oneOf(Gen.choose(1.1f, 100f), Gen.choose(-100f, -0.1f))) {q =>
        intercept[IllegalArgumentException] {Conneg("value", q)}
      }
    }

    it("should accept legal q values") {
      forAll(Gen.choose(0f, 1f)) {q =>
        Conneg("value", q)
      }
    }
  }
}