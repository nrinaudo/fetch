package com.nrinaudo.fetch

import org.scalatest.{Matchers, FunSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import Arbitrary._
import java.io.{OutputStream, InputStream, ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.Charset
import scala.io.Source
import java.util.zip.{InflaterInputStream, DeflaterOutputStream, GZIPOutputStream, GZIPInputStream}

object EncodingSpec {
  // - Generators ------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val arbEncoding: Arbitrary[Encoding] = Arbitrary(Gen.oneOf(Encoding.Gzip, Encoding.Deflate, Encoding.Identity))
  def illegalEncoding: Gen[String] = arbitrary[String].suchThat(e => !Encoding.DefaultEncodings.contains(e))



  // - Helper functions ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def encode(content: String)(implicit encoding: Encoding): Array[Byte] =
    writeAll(content)(encoding.encode)

  def decode(content: Array[Byte])(implicit encoding: Encoding): String =
    readAll(content)(encoding.decode)

  def writeAll(content: String)(f: OutputStream => OutputStream): Array[Byte] = {
    val bout = new ByteArrayOutputStream()
    val out  = f(bout)

    out.write(content.getBytes(Charset.forName("utf-8")))
    out.close()

    bout.toByteArray
  }

  def readAll(content: Array[Byte])(f: InputStream => InputStream): String =
    Source.fromInputStream(f(new ByteArrayInputStream(content)), "utf-8").mkString
}

class EncodingSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  import EncodingSpec._

  describe("An encoding") {
    it("should be able to decode content it has encoded") {
      forAll { (encoding: Encoding, content: String) =>
        implicit val e = encoding
        decode(encode(content)) should be(content)
      }
    }
  }

  describe("The GZIP encoding") {
    implicit val gzip = Encoding.Gzip

    it("should GZIP raw content") {
      forAll { content: String =>
        readAll(encode(content))(new GZIPInputStream(_)) should be(content)
      }
    }

    it("should \"un-GZIP\" GZIPed content") {
      forAll { content: String =>
        decode(writeAll(content)(new GZIPOutputStream(_))) should be(content)
      }
    }
  }

  describe("The Deflate encoding") {
    implicit val deflate = Encoding.Deflate

    it("should deflate raw content") {
      forAll { content: String =>
        readAll(encode(content))(new InflaterInputStream(_)) should be(content)
      }
    }

    it("should inflate deflated content") {
      forAll { content: String =>
        decode(writeAll(content)(new DeflaterOutputStream(_))) should be(content)
      }
    }
  }

  describe("The Identity encoding") {
    implicit val identity = Encoding.Identity

    it("should not modify read content") {
      forAll { content: String =>
        readAll(encode(content))(in => in) should be(content)
      }
    }

    it("should not modify written content") {
      forAll { content: String =>
        decode(writeAll(content)(out => out)) should be(content)
      }
    }
  }
}
