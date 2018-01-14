package ru.ifmo.ctddev.advanced_structures.zamyatin

import ru.ifmo.ctddev.advanced_structures.zamyatin.CartesianTree.Node

import scala.util.Random

object CartesianTree {
  case class Node[T](left: Option[Node[T]], right: Option[Node[T]], value: T, y: Int) {
    val size: Int = left.fold(0){_.size} + right.fold(0){_.size}
  }

  object Node {
    def apply[T](x: Int, v: T): Node[T] = Node(None, None, v, Random.nextInt())
  }
}

class CartesianTree[T] {
  private var root: Option[Node[T]] = None

  private def size(v: Option[Node[T]]) = v.fold(0){_.size}

  def this(root: Option[Node[T]]) {
    this()
    this.root = root
  }

  private def split(v: Option[Node[T]], i: Int): (Option[Node[T]], Option[Node[T]]) = {
    v.fold((Option.empty[Node[T]], Option.empty[Node[T]])) { v =>
      if (i <= size(v.left)) {
        val (l, r) = split(v.left, i)
        (l, Some(v.copy(left = r)))
      } else {
        val (l, r) = split(v.right, i)
        (Some(v.copy(right = l)), r)
      }
    }
  }

  private def merge(v1: Option[Node[T]], v2: Option[Node[T]]): Option[Node[T]] = (v1, v2) match {
    case (v@Some(a), None) => v
    case (None, v@Some(b)) => v
    case (va@Some(a), vb@Some(b)) =>
      if (a.y > b.y) Some(a.copy(right = merge(a.right, vb)))
      else Some(b.copy(left = merge(va, b.left)))
  }

  def split(i: Int): (CartesianTree[T], CartesianTree[T]) = split(root, i) match {
    case (a, b) => (new CartesianTree(a), new CartesianTree(b))
  }

  def merge(a: CartesianTree[T], b: CartesianTree[T]): CartesianTree[T] = new CartesianTree(merge(a.root, b.root))

  def insert(i: Int, x: T): CartesianTree[T] = {
    val (l, r) = split(root, i)
    new CartesianTree(merge(merge(l, Some(Node(i, x))), r))
  }
}
