package com.nrinaudo.fetch

import java.io._

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

trait EntityWriterSpec[A] extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def toEntity(str: String): A
  def writer: EntityWriter[A]
  def name: String

  describe(s"The $name writer") {
    it("should write itself properly") {
      forAll(arbitrary[String].suchThat(!_.isEmpty)) { str =>
        val out = new ByteArrayOutputStream()
        writer.write(toEntity(str), out)
        Source.fromInputStream(new ByteArrayInputStream(out.toByteArray), DefaultCharset.name()).mkString should be(str)
      }
    }
  }
}

class FileEntityWriter extends EntityWriterSpec[File] {
  private val tmpFiles = new ThreadLocal[File] {
    override def initialValue(): File = {
      val f = File.createTempFile("fetch", "request")
      f.deleteOnExit()
      f
    }
  }

  override def toEntity(str: String) = {
    val file = tmpFiles.get

    val out = new OutputStreamWriter(new FileOutputStream(file), DefaultCharset)
    try {out.write(str)}
    finally {out.close()}

    file
  }
  override def writer = EntityWriter.file(MediaType.PlainText)
  override def name   = "File Entity"
}

class StringEntityWriter extends EntityWriterSpec[String] {
  override def toEntity(str: String) = str
  override def writer                = EntityWriter.string(MediaType.PlainText)
  override def name                  = "String Entity"
}

class StreamEntityWriter extends EntityWriterSpec[InputStream] {
  override def toEntity(str: String) = new ByteArrayInputStream(str.getBytes(DefaultCharset))
  override def writer                = EntityWriter.stream(MediaType.PlainText)
  override def name                  = "Stream Entity"
}

class ReaderEntityWriter extends EntityWriterSpec[Reader] {
  override def toEntity(str: String) = new StringReader(str)
  override def writer                = EntityWriter.reader(MediaType.PlainText)
  override def name                  = "Reader Entity"
}