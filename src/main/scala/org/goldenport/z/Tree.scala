package org.goldenport.z

import scalaz._
import Scalaz._

/**
 * @since   Jan. 14, 2012
 * @version Jan. 16, 2012
 * @author  ASAMI, Tomoharu
 */
trait ZTrees {
  def traverse[T, U](tree: Tree[T],
      enter: PartialFunction[Tree[T], U])(
      leave: PartialFunction[Tree[T], U] = Map.empty[Tree[T], U])(implicit mo: Monoid[U]): U = {
    foldTraverse(tree, mo.zero)(enter, leave)
  }

  def foldTraverse[T, U: Monoid](tree: Tree[T],
      monoid: U)(
      enter: PartialFunction[Tree[T], U],
      leave: PartialFunction[Tree[T], U] = Map.empty[Tree[T], U]): U = {
    val m1 = if (enter.isDefinedAt(tree)) {
      monoid |+| enter(tree)
    } else monoid
    val m2 = tree.subForest.foldl(m1) {
      (m, t) => foldTraverse(t, m)(enter, leave)
    }
    if (leave.isDefinedAt(tree)) {
      m2 |+| leave(tree)
    } else m2
  }

  def replace[T](tree: Tree[T])(
      pf: PartialFunction[(T, Stream[Tree[T]]), (T, Stream[Tree[T]])]): Tree[T] = {
    replaceDeep(tree)(pf)
  }

  def replaceShallow[T](tree: Tree[T])(
      pf: PartialFunction[(T, Stream[Tree[T]]), (T, Stream[Tree[T]])]): Tree[T] = {
    if (pf.isDefinedAt((tree.rootLabel, tree.subForest))) {
      val (r, cs) = pf((tree.rootLabel, tree.subForest))
      if (cs.isEmpty) leaf(r)
      else r.node(cs.toArray: _*)
    } else {
      val r = tree.rootLabel
      val cs = tree.subForest
      if (cs.isEmpty) tree
      else r.node(cs.map(replaceShallow(_)(pf)).toArray: _*)
    }
  }

  def replaceDeep[T](tree: Tree[T])
      (pf: PartialFunction[(T, Stream[Tree[T]]), (T, Stream[Tree[T]])]): Tree[T] = {
    if (pf.isDefinedAt((tree.rootLabel, tree.subForest))) {
      val (r, cs) = pf((tree.rootLabel, tree.subForest))
      if (cs.isEmpty) r.leaf
      else r.node(cs.map(replaceDeep(_)(pf)).toArray: _*)
    } else {
      val r = tree.rootLabel
      val cs = tree.subForest
      if (cs.isEmpty) tree
      else r.node(cs.map(replaceDeep(_)(pf)).toArray: _*)
    }
  }

  def replaceNode[T](tree: Tree[T])
      (pf: PartialFunction[T, T]): Tree[T] = {
    if (pf.isDefinedAt(tree.rootLabel)) {
      val r = pf(tree.rootLabel)
      val cs = tree.subForest
      if (cs.isEmpty) tree
      else r.node(cs.map(replaceNode(_)(pf)).toArray: _*)
    } else {
      val r = tree.rootLabel
      val cs = tree.subForest
      if (cs.isEmpty) tree
      else r.node(cs.map(replaceNode(_)(pf)).toArray: _*)
    }
  }

  def transform[T, U](tree: Tree[T])(
      f: (T, Stream[Tree[T]]) => (U, Stream[Tree[U]])): Tree[U] = {
    val (r, cs) = f(tree.rootLabel, tree.subForest)
    if (cs.isEmpty) r.leaf
    else r.node(cs.toArray: _*)
  }

  def find[T](tree: Tree[T])(p: Tree[T] => Boolean): Option[Tree[T]] = {
    if (p(tree)) tree.some
    else tree.subForest.find(p)
  }

  def collect[T, U](tree: Tree[T])(
      pf: PartialFunction[Tree[T], U]): Stream[U] = {
    collectDeep(tree)(pf)
  }

  def collectDeep[T, U](tree: Tree[T])(
      pf: PartialFunction[Tree[T], U]): Stream[U] = {
    def children = tree.subForest.flatMap(c => collectDeep(c)(pf))
    if (pf.isDefinedAt(tree)) {
      Stream.cons(pf(tree) , children)
    } else {
      children
    }
  }

  def collectFirst[T, U](tree: Tree[T])(
      pf: PartialFunction[Tree[T], U]): Option[U] = {
    if (pf.isDefinedAt(tree)) pf(tree).some
    else tree.subForest.collectFirst(pf)
  }
}

object ZTrees extends ZTrees
