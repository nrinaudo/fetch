package com.nrinaudo.fetch

import java.text.DecimalFormat

object Conneg {
  /** Format for the `q` content-negotiation header. */
  private val qFormat = new DecimalFormat("0.###")
}

/** Represents an acceptable value for content negotiation headers (`Accept-*`).
  *
  * @param  value value of the header (its `toString` method will be used when setting HTTP headers).
  * @param  q     weight of the value, as a float between 0 and 1 inclusive. Bigger weights tell remote servers that
  *               the corresponding value is more desirable than values associated with lower weights.
  */
case class Conneg[T](value: T, q: Float = 1) {
  require(q >= 0 && q <= 1, "q must be between 0 and 1, inclusive.")

  override def toString: String =
    if(q == 1) value.toString
    else       value + ";q=" + Conneg.qFormat.format(q)

  def map[S](f: T => S): Conneg[S] = Conneg(f(value), q)
  def flatMap[S](f: T => Conneg[S]): Conneg[S] = f(value).copy(q = q)
}