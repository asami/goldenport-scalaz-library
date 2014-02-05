package org.goldenport.z

import scalaz._
import Scalaz._
import scala.util.parsing.combinator.Parsers
import org.scalatest.WordSpec
import org.scalatest.Matchers
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.goldenport.scalatest.ScalazMatchers

/*
 * @since   Feb.  2, 2012
 * @version Feb.  5, 2014
 * @author  ASAMI, Tomoharu
 */
@RunWith(classOf[JUnitRunner])
class ZTreesSpec extends WordSpec with Matchers with ScalazMatchers {
  import org.goldenport.z._
  import org.goldenport.Z._

  object NumberZPathClass extends ZPathClass[Int] {
    def name(value: Int) = value.toString
  }

  val tree = 1.node(2.node(3.leaf), 4.node(5.leaf))

  "ZTrees" should {
    "collectPath" which {
      "absolute path" in {
        val r = collectZPath(tree) {
          case n => n
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPath" which {
      "absolute path" in {
        val r = collectZPath(tree) {
          case n => n
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectPathP" which {
      "absolute path" in {
        val r = collectZPathP(tree) {
          case (p, n) => (p, n)
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPathP" which {
      "absolute path" in {
        val r = collectZPathP(tree) {
          case (p, n) => (p, n)
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectPathT" which {
      "absolute path" in {
        val r = collectZPathT(tree) {
          case n => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPathT" which {
      "absolute path" in {
        val r = collectZPathT(tree) {
          case n => n.rootLabel
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectPathPT" which {
      "absolute path" in {
        val r = collectZPathPT(tree) {
          case (p, n) => (p, n.rootLabel)
        } (NumberZPathClass)
        println(r.toList)
      }
    }
    "collectZPathPT" which {
      "absolute path" in {
        val r = collectZPathPT(tree) {
          case (p, n) => (p, n.rootLabel)
        } (NumberZPathClass)
        println(r.toList)
      }
    }
  }
}
