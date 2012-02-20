package org.goldenport.z

import scalaz._
import Scalaz._

/**
 * @since   Feb. 18, 2012
 * @version Feb. 18, 2012
 * @author  ASAMI, Tomoharu
 */
object ZUtils {
  def collectZPathPT[T, U](tree: Tree[T])(
      pf: PartialFunction[(ZPath, Tree[T]), U])
      (implicit op: ZPathClass[T]): Stream[(ZPath, U)] = {
    ZTrees.collectZPathPT(tree)(pf)(op)
  }
}
