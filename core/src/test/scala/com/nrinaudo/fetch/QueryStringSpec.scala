package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import QueryString._
import scala.util.Success

object QueryStringSpec {
  /** Returns a single query parameter. */
  def queryParam: Gen[(String, List[String])] = for {
      name   <- arbitrary[String].suchThat(!_.isEmpty)
      count  <- choose(1, 5)
      values <- listOfN(count, arbitrary[String])
    } yield (name, values)

  def queryParams: Gen[Map[String, List[String]]] = for {
    count  <- choose(1, 10)
    params <- listOfN(count, queryParam)
  } yield params.foldLeft(Map(): Map[String, List[String]]) { (map, param) => map + param}


  /** Returns a query string. */
  def query = for {
    query <- queryParams
  } yield new QueryString(query)
}

class QueryStringSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import QueryStringSpec._

  describe("A Query string") {
    it("should contain the expected parameters") {
      forAll(queryParams) { params =>
        val query = new QueryString(params)

        params foreach {param =>
          // Complete parameter lists.
          query.get[String](param._1) should be(Some(Success(param._2)))
          query.getOpt[String](param._1) should be(Some(param._2))
          query[String](param._1) should be(param._2)

          // First parameter helpers.
          query.getFirst[String](param._1) should be(Some(Success(param._2(0))))
          query.getFirstOpt[String](param._1) should be(Some(param._2(0)))
          query.first[String](param._1) should be(param._2(0))
        }
      }
    }

    it("should serialize to itself") {
      forAll(query) { query =>
        QueryString(query.toString) should be(query)
      }
    }
  }
}
