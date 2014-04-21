package com.nrinaudo.fetch

import org.scalatest.{BeforeAndAfterAll, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import com.nrinaudo.fetch.net.UrlEngine

object RequestSpec {
  def httpMethod = Gen.oneOf("GET", "POST", "PUT", "DELETE", "OPTIONS", "TRACE", "PATCH", "LINK", "UNLINK")

  def nonEmpty(gen: Gen[String]) = gen.suchThat {!_.isEmpty}

  def entity = nonEmpty(arbitrary[String])

  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- nonEmpty(arbitrary[String]).suchThat {!_.contains(':')}
    pwd <- nonEmpty(arbitrary[String]).suchThat {!_.contains(':')}
  } yield (user, pwd)

  def encoding = Gen.oneOf(Encoding.Gzip, Encoding.Deflate, Encoding.Identity)
}

class RequestSpec extends FunSpec with BeforeAndAfterAll with ShouldMatchers with GeneratorDrivenPropertyChecks {
  import RequestSpec._



  // - Test HTTP server ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val server = unfiltered.jetty.Http.anylocal.plan(TestPlan)

  implicit val engine = UrlEngine()

  def request(path: String) = Request(server.url + path)

  override def beforeAll(conf: Map[String, Any]) {
    server.start()
  }

  override def afterAll(conf: Map[String, Any]) {
    server.stop()
  }



  // - Actual tests ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  describe("A Request") {
    // - Basic tests ---------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should use the specified HTTP method") {
      forAll(httpMethod) { method =>
        request("method").method(method).apply().body.as[String] should be(method)
      }
    }

    it("should extract the correct HTTP status codes") {
      forAll(Gen.choose(200, 599)) {code =>
        request("status/" + code)().status should be(Status(code))
      }
    }



    // - Entity submission / reception ---------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should correctly read and write entity bodies, regardless of the request and response encoding") {
      forAll(entity, encoding, encoding) { (entity, reqEncoding, resEncoding) =>
        val response = request("body").acceptEncoding(resEncoding).PUT(entity.encoding(reqEncoding))

        if(resEncoding == Encoding.Identity) response.headers.get("Content-Encoding") should be(None)
        else                                 response.headers.get("Content-Encoding") should be(Some(List(resEncoding.name)))

        response.body.as[String] should be(entity)
      }
    }



    // - BasicAuth -----------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should send basic auth credentials properly") {
      forAll(authCredentials) {case (user, pwd) =>
        request("auth").auth(user, pwd)().body.as[String] should be(user + "\n" + pwd)
      }
    }
  }
}
