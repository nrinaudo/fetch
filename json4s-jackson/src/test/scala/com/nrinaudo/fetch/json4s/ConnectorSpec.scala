package com.nrinaudo.fetch.json4s

import org.scalatest.{Matchers, BeforeAndAfterAll, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary
import org.json4s.JsonAST._
import unfiltered.filter.Planify
import java.io.{OutputStreamWriter, Reader}
import unfiltered.response.{HttpResponse, ResponseWriter}
import unfiltered.request.{Path, Seg}
import com.nrinaudo.fetch._
import org.json4s.JsonAST.JString
import com.nrinaudo.fetch.Request
import com.nrinaudo.fetch.net.UrlEngine

class ReaderResponse(val reader: Reader) extends ResponseWriter {
  override def respond(res: HttpResponse[Any]): Unit = {
    res.header("Content-Type", "application/json;charset=\"" + res.charset.name() + "\"")
    super.respond(res)
  }

  override def write(writer: OutputStreamWriter) {
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
  val server = unfiltered.jetty.Http.anylocal.plan(Planify {
    unfiltered.kit.GZip {
      case req @ Path(Seg("echo" :: Nil)) => new ReaderResponse(req.reader)
    }
  })

  override def beforeAll() {
    server.start()
  }

  override def afterAll() {
    server.stop()
  }

  val request: Request[JValue] = Request(server.url + "echo").get.map(_.body.as[JValue])

  def json = for {
    str <- Arbitrary.arbitrary[String]
    d   <- Arbitrary.arbitrary[Double]
    b   <- Arbitrary.arbitrary[Boolean]
  } yield JObject(JField("string", JString(str)), JField("double", JDouble(d)), JField("boolean", new JBool(b)))

  describe("The json4s-native connector") {
    it("should serialize / deserialize as expected") {
      forAll(json) {json =>
        request(json) should be(json)
      }
    }
  }
}
