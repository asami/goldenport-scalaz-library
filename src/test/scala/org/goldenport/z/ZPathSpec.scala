package org.goldenport.z

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Jan. 31, 2012
 * @version Jan. 31, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ZPathSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  "ZPath" should {
    "creation" that {
      "absolute path" in {
        val path = ZPath("/a/b/c")
        path.isAbsolute should be (true)
        path.names should be (List("", "a", "b", "c"))
        path.toString should be ("/a/b/c")
      }
      "relative path" in {
        val path = ZPath("a/b/c")
        path.isAbsolute should be (false)
        path.names should be (List("a", "b", "c"))
        path.toString should be ("a/b/c")
      }
    }
  }
}
