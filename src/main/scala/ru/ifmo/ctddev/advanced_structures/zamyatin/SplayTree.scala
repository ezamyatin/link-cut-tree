package ru.ifmo.ctddev.advanced_structures.zamyatin

case class SplayTree[K, T](left: Option[SplayTree[K, T]], right: Option[SplayTree[K, T]], parent: Option[SplayTree[K, T]], key: K, x: T) {

  private def rotate(v: SplayTree[K, T]): SplayTree[K, T] = {
    v.parent match {
      case None => v
      case Some(SplayTree(l, _, _, _, _)) if l.fold(false){v.equals} =>
        SplayTree[K, T](v.left, v.parent.map(_.copy(left = v.right, parent = Some(v))), v.parent.flatMap(_.parent), v.key, v.x)
      case Some(SplayTree(_, r, _, _, _)) if r.fold(false){v.equals} =>
        SplayTree[K, T](v.parent.map(_.copy(right = v.left, parent = Some(v))), v.right, v.parent.flatMap(_.parent), v.key, v.x)
    }
  }

  private def isRoot(v: SplayTree[K, T]): Boolean = v.parent.isEmpty

  def splay(v: SplayTree[K, T]): SplayTree[K, T] = {
    v.parent.fold(v) {
      case parent if isRoot(parent) =>
        rotate(v)
      case parent@SplayTree(l, _, Some(SplayTree(p, _, _, _, _)), _, _) if p.contains(parent) && l.contains(v) =>
        splay(rotate(rotate(parent).left.get))
      case parent@SplayTree(_, r, Some(SplayTree(_, p, _, _, _)), _, _) if p.contains(parent) && r.contains(v) =>
        splay(rotate(rotate(parent).right.get))
      case _ =>
        splay(rotate(rotate(v)))
    }
  }

  def find(key: K)(implicit cmp: K => Ordered[K]): SplayTree[K, T] = {
    if (key < this.key) {
      left.fold(splay(this))(_.find(key))
    } else if (key > this.key) {
      right.fold(splay(this))(_.find(key))
    } else {
      splay(this)
    }
  }

  def rightmost: SplayTree[K, T] = right.fold(this)(_.rightmost)

  def leftmost: SplayTree[K, T] = left.fold(this)(_.leftmost)

  def merge(other: SplayTree[K, T]): SplayTree[K, T] = rightmost.copy(right = Some(other))

  def split(key: K)(implicit cmp: K => Ordered[K]): (Option[SplayTree[K, T]], Option[SplayTree[K, T]]) = {
    val v = find(key)
    if (v.key < key) {
      (Some(v.copy(right = None)), v.right)
    } else {
      (v.left, Some(v.copy(left = None)))
    }
  }

  def add(key: K, x: T): SplayTree[K, T] = {
    val (l, r) = split(key)
    var v = SplayTree(None, None, None, key, x)
    v = l.fold(v)(_.merge(v))
    v = r.fold(v)(v.merge(_))
    v
  }

  def remove(key: K): Option[SplayTree[K, T]] = {
    val v = find(key)
    if (v.key.equals(key)) v.left.fold(v.right)(l => v.right.fold(Some(l))(r => Some(l.merge(r))))
    else Some(v)
  }

}
