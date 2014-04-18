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
  /** Charset used for request or response entities when none is specified. */
  val         DefaultCharset = Charset.forName("UTF-8")
  /** Size of the buffer used when processing streams. */
  private val BufferSize     = 4096
  /** Represents a request or response's HTTP headers. */
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
  implicit def stringToEntity(str: String)     = RequestEntity(str)
  implicit def readerToEntity(reader: Reader)  = RequestEntity(reader)
  implicit def streamToEntity(in: InputStream) = RequestEntity(in)
  implicit def fileToEntity(file: File)        = RequestEntity(file)
  implicit def stringToURL(str: String)        = new URL(str)
  implicit def urlToURL(url: Url)              = new URL(url.toString)
}
