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
 * @since   Feb.  2, 2012
 * @version Feb.  2, 2012
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ZTreesSpec extends WordSpec with ShouldMatchers with ScalazMatchers {
  import org.goldenport.z._
  import org.goldenport.Z._

  object NumberZPathClass extends ZPathClass[Int] {
    def name(value: Int) = value.toString
  }

  val tree = 1.node(2.node(3.leaf), 4.node(5.leaf))

  "ZTrees" should {
    "collectPath" that {
      "absolute path" in {
        val r = collectZPath(tree) {
          case n => n
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPath" that {
      "absolute path" in {
        val r = collectZPath(tree) {
          case n => n
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectPathT" that {
      "absolute path" in {
        val r = collectZPathT(tree) {
          case n => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPathT" that {
      "absolute path" in {
        val r = collectZPathT(tree) {
          case n => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectPathPT" that {
      "absolute path" in {
        val r = collectZPathPT(tree) {
          case (p, n) => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPathPT" that {
      "absolute path" in {
        val r = collectZPathPT(tree) {
          case (p, n) => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
  }
}
