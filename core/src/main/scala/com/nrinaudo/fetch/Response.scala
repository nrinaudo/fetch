package com.nrinaudo.fetch

object Status {
  val Ok       = Status(200)
  val NotFound = Status(404)
}

case class Status(code: Int) {
  def isClientError = code >= 400 && code < 500
  def isServerError = code >= 500 && code < 600
  def isRedirection = code >= 300 && code < 400
  def isSuccess     = code >= 200 && code < 300
  def isError       = isClientError || isServerError
}

/**
 * @author Nicolas Rinaudo
 */
case class Response[A](status: Status, body: A) {
  def map[B](f: A => B): Response[B] = Response(status, f(body))
  def flatMap[B](f: A => Response[B]): Response[B] = Response(status, f(body).body)
  def foreach[U](f: A => U): U = f(body)
}