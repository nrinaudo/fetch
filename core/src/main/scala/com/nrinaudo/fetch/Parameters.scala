package com.nrinaudo.fetch

import fastparse.Parser

object Parameters {
  val empty: Parameters = Parameters(Map.empty)

  private[fetch] val parser: Parser[Parameters] =
    grammar.params.map(p => Parameters(p.foldLeft(Map.empty[String, String]) { case (acc, param) => acc + param }))

  def parse(str: String): Option[Parameters] =
    if(str == null) Some(Parameters.empty)
    else parseFully(parser, str)
}

/**
 * Stores a list of name / value pairs.
 *
 * Values are stored in their serialized forms as `String`, but can be manipulated as more specific types provided
 * corresponding instances of [[ValueReader]] and [[ValueWriter]] are in scope. Default implementations of these
 * can be found in [[ValueFormat$ ValueFormat]].
 */
// TODO: do we really need to split these into Headers and MediaTypeParameters
case class Parameters(values: Map[String, String]) {
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
  private def setRaw(name: String, value: String): Parameters = copy(values + (name -> value))

  /** Sets the specified parameter to the specified value.
    *
    * @param  name  name of the parameter.
    * @param  value value of the parameter.
    * @tparam T     type of the parameter. An implicit instance of [[ValueWriter]] for this type is required to be in
    *               scope.
    */
  def set[T: ValueWriter](name: String, value: T): Parameters =
    implicitly[ValueWriter[T]].write(value).fold(this)(setRaw(name, _))

  /** Sets the specified parameter to the specified value if it does not exist. Otherwise, does nothing.
    *
    * @param name  name of the parameter to set.
    * @param value desired value.
    * @tparam T    type of the parameter. An implicit instance of [[ValueWriter]] for this type is required to be in
    *              scope.
    */
  def setIfEmpty[T: ValueWriter](name: String, value: T): Parameters =
    if(contains(name)) this
    else               set(name, value)

  /** Removes the specified parameter if it exists. Otherwise, does nothing.
    *
    * @param name name of the parameter to remove.
    */
  def remove(name: String): Parameters =
    if(contains(name)) copy(values - name)
    else               this
}