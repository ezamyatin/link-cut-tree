package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.annotation.tailrec
import scala.util.Random

class SplayTree[T, V](val x: T,
                      private var left: SplayTree[T, V],
                      private var right: SplayTree[T, V],
                      private var f: SplayTree[T, V] => V) {

  private var parent: SplayTree[T, V] = this

  private var agg: V = f(this)

  private def recompute(): Unit = {
    agg = f(this)
  }

  def isRoot = parent == this

  def aggregation: V = agg

  private def doIfNotNull[E](t: SplayTree[T, V], f: (SplayTree[T, V] => Unit)) = {
    if (t != null)
      f(t)
  }

  private def rotate(): Unit = SplayTree.measure{
    val parentWasRoot = parent.isRoot

    val pp = parent.parent
    val oldParent = parent

    if (isRoot) {

    } else if (parent.left == this) {
      parent.left = right
      //right.foreach(_.parent = parent)
      doIfNotNull(right, _.parent = parent)

      parent.parent = this
      right = parent
    } else if (parent.right == this) {
      parent.right = left
      //left.foreach(_.parent = parent)
      doIfNotNull(left, _.parent = parent)

      parent.parent = this
      left = parent
    }

    if (!parentWasRoot) {
      if (pp.left == parent)
        pp.left = this
      else
        pp.right = this

      parent = pp
    } else {
      parent = this
    }


    oldParent.recompute()
    recompute()
    pp.recompute()

  }

  def splay(): Unit = {
    while (!isRoot) {
      if (parent.isRoot)
          rotate()
      else if ((parent.parent.left == (parent) && parent.left == (this)) ||
          (parent.parent.right == (parent) && parent.right == (this))) {
          parent.rotate()
          rotate()
      } else {
          rotate()
          rotate()
      }
    }
  }

  def rightmost: SplayTree[T, V] = if (right == null) this else right.rightmost//right.fold(this)(_.rightmost)

  def leftmost: SplayTree[T, V] = if (left == null) this else left.leftmost//left.fold(this)(_.leftmost)

  def merge(other: SplayTree[T, V]): SplayTree[T, V] = {if (other != this) {
    if (!isRoot) throw new IllegalArgumentException
    val v = rightmost
    v.splay()
    assert(v.isRoot)
    v.right = other
    assert(other.isRoot)
    other.parent = v
    v.recompute()
    v
  } else this}


  //def toList: List[T] = left.fold(List.empty[T])(_.toList) ++ List(x) ++ right.fold(List.empty[T])(_.toList)

  def cutLeftChild(): Unit = {
    //left.foreach(v => v.parent = v)
    doIfNotNull(left, v => v.parent = v)
    left = null//None
    recompute()
  }
  
  def cutRightChild(): Unit = {
    //right.foreach(v => v.parent = v)
    doIfNotNull(right, v => v.parent = v)
    right = null//None
    recompute()
  }

  def root: SplayTree[T, V] = {
    splay()
    this
  }

  def getLeft = left

  def getRight = right

  //override def toString: String = s"SplayTree($x,${left.map(_.x)},${right.map(_.x)},${parent.x})"

}

object SplayTree {
  def create[T, V](x: T, f: (SplayTree[T, V] => V)): SplayTree[T, V] = new SplayTree[T, V](x, null, null, f)
  var time: Double = 0
  var count = 0

  def reset() = {time = 0; count = 0}

  def measure[T](f: =>T): T = {
    count += 1
    val s = System.nanoTime()
    val x = f
    val e = System.nanoTime()
    SplayTree.time += (e-s) * 1d /1000000
    x
  }
}