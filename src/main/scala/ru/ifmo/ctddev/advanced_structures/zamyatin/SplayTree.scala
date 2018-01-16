package ru.ifmo.ctddev.advanced_structures.zamyatin

class SplayTree[T, V](val x: T,
                      private var left: Option[SplayTree[T, V]],
                      private var right: Option[SplayTree[T, V]],
                      private var f: SplayTree[T, V] => V) {

  private var parent: SplayTree[T, V] = this

  private var agg: V = f(this)

  private def recompute(): Unit = {
    agg = f(this)
  }

  def isRoot = parent == this

  def aggregation: V = agg

  private def rotate(): Unit = {
    val parentWasRoot = parent.isRoot
    val pp = parent.parent
    val oldParent = parent
    parent match {
      case _ if isRoot =>

      case _ if parent.left.contains(this)  =>

        parent.left = right
        right.foreach(_.parent = parent)

        parent.parent = this
        right = Some(parent)

      case _ if parent.right.contains(this) =>
        parent.right = left
        left.foreach(_.parent = parent)

        parent.parent = this
        left = Some(parent)
    }

    if (!parentWasRoot) {
      if (pp.left.contains(parent))
        pp.left = Some(this)
      else if (pp.right.contains(parent))
        pp.right = Some(this)
      else assert(false)

      parent = pp
    } else {
      parent = this
    }

    assert(!left.contains(parent))
    assert(!parent.left.contains(parent.parent))
    oldParent.recompute()
    recompute()
    pp.recompute()
  }

  def splay(): Unit = {
    parent match {
      case _ if parent.isRoot =>
        rotate()
      case _ if (parent.parent.left.contains(parent) && parent.left.contains(this)) ||
                  (parent.parent.right.contains(parent) && parent.right.contains(this)) =>
        parent.rotate()
        rotate()
        splay()
      case _ => 
        rotate()
        rotate()
        splay()
    }
  }

  def rightmost: SplayTree[T, V] = right.fold(this)(_.rightmost)

  def leftmost: SplayTree[T, V] = left.fold(this)(_.leftmost)

  def merge(other: SplayTree[T, V]): SplayTree[T, V] = if (other != this) {
    if (!isRoot) throw new IllegalArgumentException
    val v = rightmost
    v.splay()
    assert(v.isRoot)
    v.right = Some(other)
    assert(other.isRoot)
    other.parent = v
    v.recompute()
    v
  } else this


  def toList: List[T] = left.fold(List.empty[T])(_.toList) ++ List(x) ++ right.fold(List.empty[T])(_.toList)

  def cutLeftChild(): Unit = {
    left.foreach(v => v.parent = v)
    left = None
    recompute()
  }
  
  def cutRightChild(): Unit = {
    right.foreach(v => v.parent = v)
    right = None
    recompute()
  }

  def root: SplayTree[T, V] = if (isRoot) this else parent.root

  def getLeft: Option[SplayTree[T, V]] = left

  def getRight: Option[SplayTree[T, V]] = right

  override def toString: String = s"SplayTree($x,${left.map(_.x)},${right.map(_.x)},${parent.x})"
}

object SplayTree {
  def create[T, V](x: T, f: (SplayTree[T, V] => V)): SplayTree[T, V] = new SplayTree[T, V](x, None, None, f)
}