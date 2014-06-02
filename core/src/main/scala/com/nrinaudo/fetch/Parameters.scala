package com.nrinaudo.fetch

import scala.util.{Success, Try, Failure}

/**
 * Stores a list of name / value pairs.
 *
 * Values are stored in their serialized forms as `String`, but can be manipulated as more specific types provided
 * corresponding instances of [[ValueReader]] and [[ValueWriter]] are in scope. Default implementations of these
 * can be found in [[ValueFormat$ ValueFormat]].
 *
 * @tparam Self
 */
trait Parameters[Self <: Parameters[Self]] {
  this: Self =>


  // - Abstract members ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Where the parameter and their values are stored. */
  val values: Map[String, String]

  /** Creates a new instance of [[Parameters]] with the specified values. */
  def build(values: Map[String, String]): Self



  // - Parameter retrieval ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns the value of the requested parameter.
    *
    * This method is unsafe: it will throw an exception if the requested parameter is either absent or not found. For
    * safer alternatives, see [[Parameters.get]] and [[Parameters.getOpt]].
    *
    * @param  name name of the parameter to retrieve.
    * @tparam T    type of the parameter to retrieve. An implicit instance of [[ValueReader]] for this type is required
    *              to be in scope.
    */
  def apply[T: ValueReader](name: String): T = implicitly[ValueReader[T]].read(values(name)).get


  /** Returns the value of the requested parameter.
    *
    * This method does not differentiate between an absent parameter and one with an illegal value: both cases will
    * return `None`. If the distinction is important, use the [[Parameters.get]] method.
    *
    * @param name name of the parameter to retrieve.
    * @tparam T   type of the parameter to retrieve. An implicit instance of [[ValueReader]] for this type is required
    *             to be in scope.
    */
  def getOpt[T: ValueReader](name: String): Option[T] = for {
    raw    <- values.get(name)
    parsed <- implicitly[ValueReader[T]].read(raw)
  } yield parsed

  /** Returns the value of the requested parameter.
    *
    * This method differentiates between an absent parameter (`None`) and one with an illegal value (`Some(Failure))`.
    * If that distinction is not necessary, used [[Parameters.getOpt]] instead.
    *
    * @param name name of the parameter to retrieve.
    * @tparam T   type of the parameter to retrieve. An implicit instance of [[ValueReader]] for this type is required
    *             to be in scope.
    * @return
    */
  def get[T: ValueReader](name: String): Option[Try[T]] = values.get(name).map { v =>
    implicitly[ValueReader[T]].read(v) match {
      case Some(t) => Success(t)
      case None    => Failure(new IllegalArgumentException("Illegal value: " + name))
    }
  }

  /** Checks whether the current instance contains a parameter with the specified name. */
  def contains(name: String): Boolean = values.contains(name)



  // - Parameter modification ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Sets the specified parameter to the specified value.
    *
    * @param  name  name of the parameter.
    * @param  value value of the parameter.
    * @tparam T     type of the parameter. An implicit instance of [[ValueWriter]] for this type is required to be in
    *               scope.
    */
  def set[T: ValueWriter](name: String, value: T): Self =
    implicitly[ValueWriter[T]].write(value).fold(this)(set(name, _))

  def set(name: String, value: String): Self = build(values + (name -> value))

  /** Sets the specified parameter to the specified value if it does not exist. Otherwise, does nothing.
    *
    * @param name  name of the parameter to set.
    * @param value desired value.
    * @tparam T    type of the parameter. An implicit instance of [[ValueWriter]] for this type is required to be in
    *              scope.
    */
  def setIfEmpty[T: ValueWriter](name: String, value: T): Self =
    if(contains(name)) this
    else               set(name, value)

  /** Removes the specified parameter if it exists. Otherwise, does nothing.
    *
    * @param name name of the parameter to remove.
    */
  def remove(name: String): Self =
    if(contains(name)) build(values - name)
    else               this



  // - Object implementation -------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def toString = values.mkString("(", ",", ")")

  override def hashCode(): Int = values.hashCode()

  override def equals(p1: scala.Any): Boolean = p1 match {
    case a: Parameters[_] => a.values == values
    case _                => false
  }
}