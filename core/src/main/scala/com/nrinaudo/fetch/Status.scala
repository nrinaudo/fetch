package com.nrinaudo.fetch

object Status {
  object NotFound extends Status(404)
}

/**
 * Represents HTTP statuses.
 * @author Nicolas Rinaudo
 */
case class Status(code: Int) {
  def isClientError = code >= 400 && code < 500
  def isServerError = code >= 500 && code < 600
  def isRedirection = code >= 300 && code < 400
  def isSuccess     = code >= 200 && code < 300
  def isError       = isClientError || isServerError
}
