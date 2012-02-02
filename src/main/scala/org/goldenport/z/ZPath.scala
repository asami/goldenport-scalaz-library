package org.goldenport.z

import scalaz._
import Scalaz._

/**
 * @since   Jan. 31, 2012
 * @version Feb.  2, 2012
 * @author  ASAMI, Tomoharu
 */
case class ZPath(names: List[String]) {
  def /(name: String) = {
    if (name == null) {
      println("null")
    }
    require (name != null && name.nonEmpty, "ZPath#/: name component must not be empty.")
    ZPath(names ::: ZPath.string2names(name))
  }

  def /(path: ZPath) = {
    ZPath(names ::: path.names)
  }

  // first component is "" means absolute path
  def isAbsolute: Boolean = names.headOption.cata(_.isEmpty, false)

  override def toString(): String = qname

  def qname: String = {
    if (names.length == 1 && names.head.isEmpty) "/"
    else names.mkString("/")
  }

  def suffix: Option[String] = {
    names.lastOption.flatMap { n =>
      n.lastIndexOf('.') match {
        case i if i < n.length - 1=> n.substring(i + 1).some
        case _ => none
      }
    }
  }
}

object ZPath {
  implicit def zpathWrapper(s: String): ZPath = ZPath(List(s))
  implicit def zpathWrapper(s: Symbol): ZPath = ZPath(List(s.name))

  def apply(names: String): ZPath = {
    if (names == null) ZPath(List(""))
    else ZPath(string2names(names))
  }

  def string2names(names: String) = {
    names.split("/").toList
  }
}

trait ZPathClass[M] {
  def name(value: M): String
}

object ZPaths {
}

object ZPathRoot extends ZPath(List(""))
