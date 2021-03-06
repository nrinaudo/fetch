package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen._
import org.scalacheck.Arbitrary._
import QueryString._
import scala.util.Success

object QueryStringSpec {
  /** Returns a single query parameter. */
  def queryParam: Gen[(String, List[String])] = for {
    name   <- arbitrary[String].suchThat(!_.isEmpty)
    count  <- choose(1, 5)
    values <- listOfN(count, arbitrary[String].suchThat(!_.isEmpty))
  } yield (name, values)

  def queryParams: Gen[Map[String, List[String]]] = for {
    count  <- choose(1, 10)
    params <- listOfN(count, queryParam)
  } yield params.foldLeft(Map(): Map[String, List[String]]) { (map, param) => map + param}

  /** Returns a query string. */
  def query = for {
    query <- queryParams
  } yield QueryString(query)
}

class QueryStringSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import QueryStringSpec._

  describe("A Query string") {
    it("should contain the expected parameters") {
      forAll(queryParams) { params =>
        val query = QueryString(params)

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
        QueryString('?' + query.toString) should be(query)
      }
    }

    it("should set parameters as expected") {
      forAll(queryParam) { param =>
        QueryString().set(param._1, param._2: _*) should be(QueryString(Map(param)))
      }
    }

    it("should get parameters as expected") {
      forAll(queryParams) { params =>
        val query = QueryString(params)

        params.foreach { case (name, values) =>
          // Found.
          query.get[String](name) should be(params.get(name).map(Success(_)))
          query.getOpt[String](name) should be(params.get(name))
          query[String](name) should be(params(name))

          // Not found.
          val trimmed = QueryString(params - name)
          trimmed.get[String](name) should be(None)
          trimmed.getOpt[String](name) should be(None)
          intercept[NoSuchElementException] { trimmed[String](name) }

          // Incorrect type.
          val modified = QueryString(params + (name -> List("abc")))
          modified.get[Int](name).get.isFailure should be(true)
          modified.getOpt[Int](name) should be(None)
          intercept[IllegalArgumentException] { modified[Int](name) }
        }
      }
    }

    it("should add parameters as expected") {
      forAll(queryParams) { params =>
        // Adds all values at once.
        params.foldLeft(QueryString()) {(query, param) =>
          query.add(param._1, param._2: _*)
        } should be(QueryString(params))

        // Adds values one after the other.
        params.foldLeft(QueryString()) { (query, param) =>
          param._2.foldLeft(query) { (query, value) => query.add(param._1, value)}
        } should be(QueryString(params))

        // Single parameter values.
        params.mapValues(_.head).foldLeft(QueryString()) { (query, param) =>
          query & param
        } should be(QueryString(params.mapValues(l => List(l.head))))
      }
    }

    it("should ignore empty parameters at creation time") {
      forAll(queryParams, identifier) { (params, name) =>
        QueryString(params + (name -> List(""))).values should be(params)

        QueryString(Map(name -> Nil)) should be(QueryString())
      }
    }

    it("should ignore empty parameters at modification time") {
      forAll(queryParams, identifier) { (params, name) =>
        val query = QueryString(params)

        query.set(name, "")  should be(query)
        query.add(name, "")  should be(query)
        (query & name -> "") should be(query)

        QueryString().set(name, List[String](): _*) should be(QueryString())
        QueryString().add(name, List[String](): _*) should be(QueryString())
      }
    }

    it("should have working hashCode and equals implementation") {
      forAll(query, query) { (q1, q2) =>
        // Simple equality tests.
        q1 should be(q1)
        q1.hashCode should be(q1.hashCode)

        // Equality after clone.
        QueryString(q1.values) should be(q1)
        QueryString(q1.values).hashCode should be(q1.hashCode)

        // Non-equality tests.
        if(q1.values != q2.values) {
          q1 should not be(q2)
          q1.hashCode should not be(q2.hashCode)
        }

        q1 should not be(q1.values)
      }
    }
  }
}
