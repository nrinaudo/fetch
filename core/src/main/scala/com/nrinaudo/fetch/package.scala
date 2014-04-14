package com.nrinaudo

import java.io._
import java.nio.charset.Charset
import scala.language.implicitConversions
import java.net.URL

/**
 * @author Nicolas Rinaudo
 */
package object fetch {
  // - Package constants -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  val         DefaultCharset = Charset.forName("UTF-8")
  private val BufferSize     = 4096

  type Headers = Map[String, List[String]]



  // - IO helper methods -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Writes the content of the specified input stream to the specified output stream.
    * Note that this method does not wrap its arguments in buffered streams, nor will it close either stream.
    */
  def writeBytes(in: InputStream, out: OutputStream) {
    def loop(buffer: Array[Byte]): Unit =
      in.read(buffer, 0, buffer.length) match {
        case count if count > 0 =>
          out.write(buffer, 0, count)
          loop(buffer)
        case _ =>
      }
    loop(new Array[Byte](BufferSize))
  }

  /** Writes the content of the specified reader to the specified writer.
    * Note that this method does not wrap its arguments in buffered implementations, nor will it close either stream.
    */
  def writeChars(in: Reader, out: Writer) = {
    def loop(buffer: Array[Char]): Unit =
      in.read(buffer, 0, buffer.length) match {
        case count if count > 0 =>
          out.write(buffer, 0, count)
          loop(buffer)
        case _ =>
      }
    loop(new Array[Char](BufferSize))
  }


  // - Implicit conversions --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit def stringToEntity(str: String)     = new StringEntity(str)
  implicit def readerToEntity(reader: Reader)  = new ReaderEntity(reader)
  implicit def streamToEntity(in: InputStream) = new StreamEntity(in)
  implicit def stringToURL(str: String)        = new URL(str)
}
