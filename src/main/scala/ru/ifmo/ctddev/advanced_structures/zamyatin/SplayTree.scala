package ru.ifmo.ctddev.advanced_structures.zamyatin

class SplayTree(val x: Vertex,
                private var left: SplayTree,
                private var right: SplayTree) {

  private var parent: SplayTree = this

  private var pathRoot: Vertex = x

  private var size: Int = 1

  @inline private def recompute(): Unit = {
    pathRoot = if (left == null) x else left.pathRoot
    size = (if (left == null) 0 else left.size) + 1 + (if (right == null) 0 else right.size)
  }

  def isRoot = parent == this

  def aggregation: (Vertex, Int) = (x, size)

  private def doIfNotNull[E](t: SplayTree, f: (SplayTree => Unit)) = {
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

  def rightmost: SplayTree = if (right == null) this else right.rightmost//right.fold(this)(_.rightmost)

  def leftmost: SplayTree = if (left == null) this else left.leftmost//left.fold(this)(_.leftmost)

  def merge(other: SplayTree): SplayTree = {if (other != this) {
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

  def root: SplayTree = {
    splay()
    this
  }

  def getLeft = left

  def getRight = right

  //override def toString: String = s"SplayTree($x,${left.map(_.x)},${right.map(_.x)},${parent.x})"

}

object SplayTree {
  def create(x: Vertex): SplayTree = new SplayTree(x, null, null)
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