package com.nrinaudo.fetch

import java.io._

import org.scalacheck.Arbitrary._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.io.Source

abstract class EntityWriterSpec[A: EntityWriter] extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {
  def toEntity(str: String): A
  def name: String

  describe(s"The $name writer") {
    it("should write itself properly") {
      forAll(arbitrary[String].suchThat(!_.isEmpty)) { str =>
        val out = new ByteArrayOutputStream()
        EntityWriter[A].write(toEntity(str), out)
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
  override def name   = "File Entity"
}

class StringEntityWriter extends EntityWriterSpec[String] {
  override def toEntity(str: String) = str
  override def name                  = "String Entity"
}

class StreamEntityWriter extends EntityWriterSpec[InputStream] {
  override def toEntity(str: String) = new ByteArrayInputStream(str.getBytes(DefaultCharset))
  override def name                  = "Stream Entity"
}

class ReaderEntityWriter extends EntityWriterSpec[Reader] {
  override def toEntity(str: String) = new StringReader(str)
  override def name                  = "Reader Entity"
}