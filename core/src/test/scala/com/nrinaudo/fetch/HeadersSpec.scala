package com.nrinaudo.fetch

import org.scalacheck.{Arbitrary, Gen}
import Gen._
import java.util.Date

object HeadersSpec {
  def date = for(time <- choose(0, 253402300799000l)) yield new Date((time / 1000l) * 1000)

  def illegalDate = Arbitrary.arbitrary[String].suchThat(_.matches(".*[^0-9a-zA-Z,: ].*"))

  def headers[T](gen: Gen[T]) = for {
    size <- Gen.choose(1, 10)
    list <- Gen.listOfN(size, gen)
  } yield list
}
