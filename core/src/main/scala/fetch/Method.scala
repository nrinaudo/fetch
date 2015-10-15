package fetch

import java.util.Locale

object Method {
  private val MethodPattern = """(\p{Alpha}+)""".r

  val GET: Method = apply("GET")
  val POST: Method = apply("POST")
  val PUT: Method = apply("PUT")
  val DELETE: Method = apply("DELETE")
  val HEAD: Method = apply("HEAD")
  val OPTIONS: Method = apply("OPTIONS")
  val TRACE: Method = apply("TRACE")
  val CONNECT: Method = apply("CONNECT")
  val PATCH: Method = apply("PATCH")
  val LINK: Method = apply("LINK")
  val UNLINK: Method = apply("UNLINK")

  def apply(value: String): Method = parse(value) getOrElse {throw new IllegalArgumentException("Not a valid method: " + value)}

  def unapply(method: Method): Option[String] = Some(method.name)

  def parse(value: String): Option[Method] = value match {
    case MethodPattern(m) => Some(new MethodImpl(m.toUpperCase(Locale.ENGLISH)))
    case _ => None
  }

  private class MethodImpl(override val name: String) extends Method {
    override def toString = name

    override def hashCode(): Int = name.hashCode

    override def equals(obj: scala.Any): Boolean = obj match {
      case m: MethodImpl => m.name == name
      case _ => false
    }
  }
}

sealed trait Method {
  val name: String
}
