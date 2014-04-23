package com.nrinaudo.fetch

import org.scalacheck.Gen
import java.nio.charset.Charset
import java.util.Locale
import org.scalacheck.Arbitrary._
import scala.collection.JavaConverters._

object ConnegSpec {
  def encoding: Gen[Encoding] = Gen.oneOf(Encoding.Gzip, Encoding.Deflate, Encoding.Identity)

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