package com.nrinaudo.fetch

/**
 * Stores a list of name / value pairs.
 *
 * Values are stored in their serialized forms as `String`, but can be manipulated as more specific types provided
 * corresponding instances of [[ValueReader]] and [[ValueWriter]] are in scope. Default implementations of these
 * can be found in [[ValueFormat$ ValueFormat]].
 */
// TODO: do we really need to split these into Headers and MediaTypeParameters
trait Parameters[Self <: Parameters[Self]] {
  this: Self =>


  // - Abstract members ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Where the parameter and their values are stored. */
  def values: Map[String, String]

  /** Creates a new instance of [[Parameters]] with the specified values. */
  protected def build(values: Map[String, String]): Self



  // - Parameter retrieval ---------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Returns the value of the requested parameter.
    *
    * This method will only return `Some` if a parameter with the specified name and expected type is found. That is,
    * if a parameter with the specified name exists, but is not in the expected type, `None` will be returned.
    *
    * @param name name of the parameter to retrieve.
    * @tparam T   type of the parameter to retrieve. An implicit instance of [[ValueReader]] for this type is required
    *             to be in scope.
    */
  def get[T: ValueReader](name: String): Option[T] = for {
    raw    <- values.get(name)
    parsed <- implicitly[ValueReader[T]].read(raw)
  } yield parsed


  /** Checks whether the current instance contains a parameter with the specified name. */
  def contains(name: String): Boolean = values.contains(name)



  // - Parameter modification ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  private def setRaw(name: String, value: String): Self = build(values + (name -> value))

  /** Sets the specified parameter to the specified value.
    *
    * @param  name  name of the parameter.
    * @param  value value of the parameter.
    * @tparam T     type of the parameter. An implicit instance of [[ValueWriter]] for this type is required to be in
    *               scope.
    */
  def set[T: ValueWriter](name: String, value: T): Self =
    implicitly[ValueWriter[T]].write(value).fold(this)(setRaw(name, _))

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