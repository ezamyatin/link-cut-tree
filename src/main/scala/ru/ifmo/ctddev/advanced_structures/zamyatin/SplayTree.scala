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

  def aggregation: V = agg

  private def rotate(): Unit = {
    val pp = parent.parent
    parent match {
      case _ if parent == this => this

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
    if (pp != parent) {
      if (pp.left.contains(parent))
        pp.left = Some(this)
      else
        pp.right = Some(this)
      assert(parent != pp)
      parent = pp
    } else {
      parent = this
    }

    assert(!left.contains(parent))
    assert(!parent.left.contains(parent.parent))
    parent.recompute()
    recompute()
    pp.recompute()
  }

  def splay(): Unit = {
    parent match {
      case _ if parent.parent == parent =>
        rotate()
      case _ if (parent.parent.left.contains(parent) && parent.left.contains(this)) ||
                  parent.parent.right.contains(parent) && parent.right.contains(this) =>
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

  def merge(other: SplayTree[T, V]): SplayTree[T, V] = {
    val v = rightmost
    v.splay()
    v.right = Some(other)
    other.parent = v
    v.recompute()
    v
  }

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

  def root: SplayTree[T, V] = if (parent == this) this else parent.root

  def getLeft: Option[SplayTree[T, V]] = left

  def getRight: Option[SplayTree[T, V]] = right

  override def toString: String = s"SplayTree($x,${left.map(_.x)},${right.map(_.x)},${parent.x})"
}

object SplayTree {
  def create[T, V](x: T, f: (SplayTree[T, V] => V)): SplayTree[T, V] = new SplayTree[T, V](x, None, None, f)
}

/*
case class SplayTree[K, T](left: Option[SplayTree[K, T]], right: Option[SplayTree[K, T]], var parent: Option[SplayTree[K, T]], key: K, x: T)
                          (implicit ev: K => Ordered[K]) {

  private def rotate: SplayTree[K, T] = {
    val v = this
    v.parent match {
      case None => v
      case Some(SplayTree(l, _, _, _, _)) if l.contains(v) =>
        val result = SplayTree[K, T](v.left, v.parent.map(_.copy(left = v.right, parent = None)), v.parent.flatMap(_.parent), v.key, v.x)
        result.right.map{e => e.parent = Some(result); e}
        result
      case Some(SplayTree(_, r, _, _, _)) if r.contains(v) =>
        val result = SplayTree[K, T](v.parent.map(_.copy(right = v.left, parent = None)), v.right, v.parent.flatMap(_.parent), v.key, v.x)
        result.left.map{e => e.parent = Some(result); e}
        result
    }
  }

  private def isRoot(v: SplayTree[K, T]): Boolean = v.parent.isEmpty

  def splay: SplayTree[K, T] = {
    parent.fold(this) {
      case parent if isRoot(parent) =>
        rotate
      case parent@SplayTree(l, _, Some(SplayTree(p, _, _, _, _)), _, _) if p.contains(parent) && l.contains(this) =>
        parent.rotate.left.get.rotate.splay
      case parent@SplayTree(_, r, Some(SplayTree(_, p, _, _, _)), _, _) if p.contains(parent) && r.contains(this) =>
        parent.rotate.right.get.rotate.splay
      case _ => rotate.rotate.splay
    }
  }

  def find(key: K): SplayTree[K, T] = {
    if (key < this.key) {
      left.fold(splay)(_.find(key))
    } else if (key > this.key) {
      right.fold(splay)(_.find(key))
    } else {
      splay
    }
  }

  def rightmost: SplayTree[K, T] = right.fold(this)(_.rightmost)

  def leftmost: SplayTree[K, T] = left.fold(this)(_.leftmost)

  def merge(other: SplayTree[K, T]): SplayTree[K, T] = {
    val newRoot = rightmost.splay
    val result = newRoot.copy(right = Some(other.copy(parent=None)))
    result.right.map{e => e.parent = Some(result); e}
    result
  }

  def split(key: K): (Option[SplayTree[K, T]], Option[SplayTree[K, T]]) = {
    val v = find(key)
    if (v.key < key) {
      (Some(v.copy(right = None)), v.right.map(_.copy(parent=None)))
    } else {
      (v.left.map(_.copy(parent=None)), Some(v.copy(left = None)))
    }
  }

  def add(key: K, x: T): SplayTree[K, T] = {
    val (l, r) = split(key)
    var v = SplayTree.create(key, x)
    v = l.fold(v)(_.merge(v))
    v = r.fold(v)(v.merge(_))
    v
  }

  def remove(key: K): Option[SplayTree[K, T]] = {
    val v = find(key)
    if (v.key.equals(key)) v.left.fold(v.right)(l => v.right.fold(Some(l))(r => Some(l.merge(r))))
    else Some(v)
  }

  def toList: List[(K, T)] = left.fold(List.empty[(K, T)])(_.toList) ++ List((key, x)) ++ right.fold(List.empty[(K, T)])(_.toList)

  override def toString: String = s"SplayTree($left,$right,${parent.map(p => (p.key, p.x))},$key,$x)"

}
*/

/*
package ru.ifmo.ctddev.advanced_structures.zamyatin

class SplayTree[T, V](val x: T,
                      private var left: Option[SplayTree[T, V]],
                      private var right: Option[SplayTree[T, V]],
                      private var f: SplayTree[T, V] => V) {

  private var parent: Option[SplayTree[T, V][] = None

  private var agg: V = f(this)

  private def recompute(): Unit = {
    agg = f(this)
  }

  def aggregation: V = agg

  private def rotate(): Unit = {
    val pp = parent.flatMap(_.parent)
    parent match {
      case _ if parent.contains(this) => this

      case _ if parent.flatMap(_.left).contains(this)  =>

        parent.foreach(_.left = right)
        right.foreach(_.parent = parent)

        parent.foreach(_.parent = Some(this))
        right = parent

        if (pp.flatMap(_.left).equals(parent))
          pp.foreach(_.left = Some(this))
        else
          pp.foreach(_.right = Some(this))
        assert(parent != pp)
        parent = pp

      case _ if parent.flatMap(_.right).contains(this) =>
        parent.foreach(_.right = left)
        right.foreach(_.parent = parent)

        parent.foreach(_.parent = Some(this))
        left = parent

        if (pp.flatMap(_.left).equals(parent))
          pp.foreach(_.left = Some(this))
        else
          pp.foreach(_.right = Some(this))
        assert(parent != pp)
        parent = pp
    }
    //assert(!left.contains(parent))
    //assert(!parent.left.contains(parent.parent))
    parent.foreach(_.recompute())
    recompute()
    pp.foreach(_.recompute())
  }

  def splay(): Unit = {
    parent match {
      case _ if parent.flatMap(_.parent).isEmpty =>
        rotate()
      case _ if (parent.flatMap(_.parent.flatMap(_.left)).contains(parent) && parent.flatMap(_.left).contains(this)) ||
                  parent.parent.right.contains(parent) && parent.right.contains(this) =>
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

  def merge(other: SplayTree[T, V]): Unit = {
    val v = rightmost
    v.splay()
    v.right = Some(other)
    other.parent = v

    v.recompute()
  }

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

  def root: SplayTree[T, V] = if (parent == this) this else parent.root

  def getLeft: Option[SplayTree[T, V]] = left

  def getRight: Option[SplayTree[T, V]] = right

  override def toString: String = s"SplayTree($x,${left.map(_.x)},${right.map(_.x)})"
}

object SplayTree {
  def create[T, V](x: T, f: (SplayTree[T, V] => V)): SplayTree[T, V] = new SplayTree[T, V](x, None, None, f)
}

/*
case class SplayTree[K, T](left: Option[SplayTree[K, T]], right: Option[SplayTree[K, T]], var parent: Option[SplayTree[K, T]], key: K, x: T)
                          (implicit ev: K => Ordered[K]) {

  private def rotate: SplayTree[K, T] = {
    val v = this
    v.parent match {
      case None => v
      case Some(SplayTree(l, _, _, _, _)) if l.contains(v) =>
        val result = SplayTree[K, T](v.left, v.parent.map(_.copy(left = v.right, parent = None)), v.parent.flatMap(_.parent), v.key, v.x)
        result.right.map{e => e.parent = Some(result); e}
        result
      case Some(SplayTree(_, r, _, _, _)) if r.contains(v) =>
        val result = SplayTree[K, T](v.parent.map(_.copy(right = v.left, parent = None)), v.right, v.parent.flatMap(_.parent), v.key, v.x)
        result.left.map{e => e.parent = Some(result); e}
        result
    }
  }

  private def isRoot(v: SplayTree[K, T]): Boolean = v.parent.isEmpty

  def splay: SplayTree[K, T] = {
    parent.fold(this) {
      case parent if isRoot(parent) =>
        rotate
      case parent@SplayTree(l, _, Some(SplayTree(p, _, _, _, _)), _, _) if p.contains(parent) && l.contains(this) =>
        parent.rotate.left.get.rotate.splay
      case parent@SplayTree(_, r, Some(SplayTree(_, p, _, _, _)), _, _) if p.contains(parent) && r.contains(this) =>
        parent.rotate.right.get.rotate.splay
      case _ => rotate.rotate.splay
    }
  }

  def find(key: K): SplayTree[K, T] = {
    if (key < this.key) {
      left.fold(splay)(_.find(key))
    } else if (key > this.key) {
      right.fold(splay)(_.find(key))
    } else {
      splay
    }
  }

  def rightmost: SplayTree[K, T] = right.fold(this)(_.rightmost)

  def leftmost: SplayTree[K, T] = left.fold(this)(_.leftmost)

  def merge(other: SplayTree[K, T]): SplayTree[K, T] = {
    val newRoot = rightmost.splay
    val result = newRoot.copy(right = Some(other.copy(parent=None)))
    result.right.map{e => e.parent = Some(result); e}
    result
  }

  def split(key: K): (Option[SplayTree[K, T]], Option[SplayTree[K, T]]) = {
    val v = find(key)
    if (v.key < key) {
      (Some(v.copy(right = None)), v.right.map(_.copy(parent=None)))
    } else {
      (v.left.map(_.copy(parent=None)), Some(v.copy(left = None)))
    }
  }

  def add(key: K, x: T): SplayTree[K, T] = {
    val (l, r) = split(key)
    var v = SplayTree.create(key, x)
    v = l.fold(v)(_.merge(v))
    v = r.fold(v)(v.merge(_))
    v
  }

  def remove(key: K): Option[SplayTree[K, T]] = {
    val v = find(key)
    if (v.key.equals(key)) v.left.fold(v.right)(l => v.right.fold(Some(l))(r => Some(l.merge(r))))
    else Some(v)
  }

  def toList: List[(K, T)] = left.fold(List.empty[(K, T)])(_.toList) ++ List((key, x)) ++ right.fold(List.empty[(K, T)])(_.toList)

  override def toString: String = s"SplayTree($left,$right,${parent.map(p => (p.key, p.x))},$key,$x)"

}
*/

 */