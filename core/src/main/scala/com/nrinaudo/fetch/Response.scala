package com.nrinaudo.fetch

/**
 * Represents an HTTP response.
 * @author Nicolas Rinaudo
 */
case class Response[A](status: Status, headers: Headers, body: A) {
  def map[B](f: A => B): Response[B] = copy(body = f(body))
  def flatMap[B](f: A => Response[B]): Response[B] = copy(body = f(body).body)
  def foreach[U](f: A => U): U = f(body)
}