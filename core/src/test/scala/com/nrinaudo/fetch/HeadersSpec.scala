package com.nrinaudo.fetch

import org.scalacheck.Gen

object HeadersSpec {
  def headers[T](gen: Gen[T]): Gen[List[T]] = for {
    size <- Gen.choose(1, 5)
    list <- Gen.listOfN(size, gen)
  } yield list
}
