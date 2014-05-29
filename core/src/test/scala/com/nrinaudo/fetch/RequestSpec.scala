package com.nrinaudo.fetch

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import Gen._
import com.nrinaudo.fetch.net.UrlEngine

object RequestSpec {
  // Note that this is not entirely correct: according to the RFC, password are allowed to contain a ':'. This is not
  // properly handled in version 0.7.1 of unfiltered, however (the issue is fixed in github, but not yet released).
  def authCredentials = for {
    user <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
    pwd  <- arbitrary[String].suchThat {str => !(str.isEmpty || str.contains(':'))}
  } yield (user, pwd)
}

class RequestSpec extends FunSpec with BeforeAndAfterAll with Matchers with GeneratorDrivenPropertyChecks {
  import MethodSpec._
  import UrlSpec._
  import QueryStringSpec._

  implicit val engine = UrlEngine()

  describe("A Request") {
    // - Method tests --------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should have a working 'method' method") {
      forAll(httpMethod, url) { (method, url) => Request(url).method(method).method should be(method) }
    }

    it("should have working method helpers") {
      forAll(url) { url =>
        val request = Request(url)

        request.GET.method should be(Method.GET)
        request.POST.method should be(Method.POST)
        request.PUT.method should be(Method.PUT)
        request.DELETE.method should be(Method.DELETE)
        request.HEAD.method should be(Method.HEAD)
        request.OPTIONS.method should be(Method.OPTIONS)
        request.TRACE.method should be(Method.TRACE)
        request.CONNECT.method should be(Method.CONNECT)
        request.PATCH.method should be(Method.PATCH)
        request.LINK.method should be(Method.LINK)
        request.UNLINK.method should be(Method.UNLINK)
      }
    }



    // - Url tests -----------------------------------------------------------------------------------------------------
    // -----------------------------------------------------------------------------------------------------------------
    it("should have a working url method") {
      forAll(url, url) { (url1, url2) => Request(url1).url(url2).url should be(url2) }
    }

    it("should have a working / method") {
      forAll(url, segment) { (url, segment) => (Request(url) / segment).url should be(url / segment) }
    }

    it("should have a working ? method") {
      forAll(url, query) { (url, query) => (Request(url) ? query).url should be(url ? query) }
    }

    it("should have a working & method") {
      import QueryString._
      forAll(url, identifier, arbitrary[String]) { (url, name, value) =>
        (Request(url) & (name, value)).url should be(url & (name -> value))
      }
    }
  }
}
