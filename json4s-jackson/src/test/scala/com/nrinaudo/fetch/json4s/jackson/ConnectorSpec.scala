package com.nrinaudo.fetch.json4s.jackson

import java.io.{OutputStreamWriter, Reader}

import com.nrinaudo.fetch.Request
import com.nrinaudo.fetch.net.UrlEngine
import org.json4s.JsonAST.{JString, _}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{BeforeAndAfterAll, FunSpec, Matchers}
import unfiltered.filter.Planify
import unfiltered.request.{Path, Seg}
import unfiltered.response.{HttpResponse, ResponseWriter}

class ReaderResponse(val reader: Reader) extends ResponseWriter {
  override def respond(res: HttpResponse[Any]): Unit = {
    res.header("Content-Type", "application/json;charset=\"" + res.charset.name() + "\"")
    super.respond(res)
  }

  override def write(writer: OutputStreamWriter): Unit = {
    var c = -1
    while({c = reader.read; c >= 0}) {
      writer.write(c)
    }
    reader.close()
  }
}

class ConnectorSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks with BeforeAndAfterAll {
  implicit val formats = org.json4s.DefaultFormats
  implicit val engine = UrlEngine()
  val server = unfiltered.jetty.Server.anylocal.plan(Planify {
    unfiltered.kit.GZip {
      case req @ Path(Seg("echo" :: Nil)) => new ReaderResponse(req.reader)
    }
  })

  override def beforeAll(): Unit = {
    server.start()
    ()
  }

  override def afterAll(): Unit = {
    server.stop()
    ()
  }

  val request: Request[JValue] = Request.from("http://localhost:" + server.ports.head + "/echo").get.map(_.body.as[JValue])

  def json: Gen[JValue] = for {
    str <- Arbitrary.arbitrary[String]
    d   <- Arbitrary.arbitrary[Double]
    b   <- Arbitrary.arbitrary[Boolean]
  } yield JObject(JField("string", JString(str)), JField("double", JDouble(d)), JField("boolean", new JBool(b)))

  describe("The json4s-native connector") {
    it("should serialize / deserialize as expected") {
      forAll(json) { json =>
        request(json) should be(json)
      }
    }
  }
}
