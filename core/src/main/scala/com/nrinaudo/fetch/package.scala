package com.nrinaudo

import java.io._
import java.nio.charset.Charset
import java.util.Locale

import com.nrinaudo.fetch.Request.HttpEngine
import fastparse.Parser
import fastparse.Parser.End
import fastparse.Result.Success

import scala.language.implicitConversions

package object fetch {
  // - Package constants -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Charset used for request or response entities when none is specified. */
  val         DefaultCharset = Charset.forName("UTF-8")
  /** Size of the buffer used when processing streams. */
  private val BufferSize     = 4096



  // - Parsing helpers -------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private[fetch] def parseFully[T](parser: Parser[T], str: String): Option[T] = (parser ~ End).parse(str, 0, false) match {
    case Success(t, _) => Some(t)
    case _             => None
  }


  // - IO helper methods -----------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Writes the content of the specified input stream to the specified output stream.
    * Note that this method does not wrap its arguments in buffered streams, nor will it close either stream.
    */
  def writeBytes(in: InputStream, out: OutputStream): Unit = {
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
}
