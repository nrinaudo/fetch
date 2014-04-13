package com.nrinaudo

import java.io._
import java.nio.charset.Charset
import scala.language.implicitConversions
import java.net.URL

/**
 * @author Nicolas Rinaudo
 */
package object fetch {
  val         DefaultCharset = Charset.forName("UTF-8")
  private val BufferSize     = 2048


  private def withClose[U](c: Closeable)(a: => U) {
    try {a}
    finally {c.close()}
  }

  /** Writes the content of the specified input stream to the specified output stream.
    * Note that this method does not wrap its arguments in buffered streams. It will, however, close both its arguments
    * upon completion (whether successful or not).
    */
  def writeAll(in: InputStream, out: OutputStream) {
    def loop(buffer: Array[Byte]): Unit =
      in.read(buffer, 0, buffer.length) match {
        case count if count  > 0 =>
          out.write(buffer, 0, count)
          loop(buffer)
        case _ =>
      }

    withClose(in) {
      withClose(out) {
        loop(new Array[Byte](BufferSize))
      }
    }
  }

  /** Writes the content of the specified reader to the specified writer.
    * Note that this method does not wrap its arguments in buffered implementations. It will, however, cloe both its
    * arguments upon completion (whether successful or not).
      */
  def writeAll(in: Reader, out: Writer) = {
    def loop(buffer: Array[Char]): Unit =
      in.read(buffer, 0, buffer.length) match {
        case count if count  > 0 =>
          out.write(buffer, 0, count)
          loop(buffer)
        case _ =>
      }

    withClose(in) {
      withClose(out) {
        loop(new Array[Char](BufferSize))
      }
    }
  }


  // - Implicit conversions --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit def stringToEntity(str: String)     = new StringEntity(str)
  implicit def readerToEntity(reader: Reader)  = new ReaderEntity(reader)
  implicit def streamToEntity(in: InputStream) = new StreamEntity(in)
  implicit def stringToURL(str: String)        = new URL(str)
}
