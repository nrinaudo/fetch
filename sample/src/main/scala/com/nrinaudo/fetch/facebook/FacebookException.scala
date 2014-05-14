package com.nrinaudo.fetch.facebook

import org.json4s.DefaultFormats
import org.json4s.JsonAST.JValue

object FacebookException {
  implicit val JsonFormat = DefaultFormats
  private def message(value: JValue) = (value \ "message").extractOrElse("Unknown error")
  private def code(value: JValue) = (value \ "code").extractOrElse(-1)
}

class FacebookException(message: String, val code: Int) extends Exception(message) {
  def this(value: JValue) = this(FacebookException.message(value), FacebookException.code(value))
}
