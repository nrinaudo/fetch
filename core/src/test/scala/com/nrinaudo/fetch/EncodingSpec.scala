package com.nrinaudo.fetch

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.nio.charset.Charset
import java.util.zip.{DeflaterOutputStream, GZIPInputStream, GZIPOutputStream, InflaterInputStream}

import com.nrinaudo.fetch.Generators._
import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

class EncodingSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

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
