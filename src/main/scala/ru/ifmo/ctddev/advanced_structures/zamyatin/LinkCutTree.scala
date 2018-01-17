package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.collection.mutable.ArrayBuffer

trait LinkCutTree {
  def add(): Int

  def link(v: Int, w: Int): Unit

  def cut(v: Int): Unit

  def findRoot(v: Int): Int

  def findRootSlow(v: Int): Int

  def getDepth(v: Int): Int

}

class Vertex(var id: Int) {
  var parent: Vertex = this
  var auxTree: SplayTree = SplayTree.create(this)
  def isRoot = parent == this
  override def toString: String = s"Vertex($id,${parent.id})"
}

class LinkCutTreeFast extends LinkCutTree {

  protected var vertices: ArrayBuffer[Vertex] = ArrayBuffer.empty

  def add(): Int = {
    vertices += new Vertex(vertices.length)
    vertices.length - 1
  }

  def link(v: Int, w: Int): Unit = {
    if (!vertices(v).isRoot) throw new IllegalArgumentException
    if (findRoot(w) == v) throw new IllegalArgumentException
    access(v)
    access(w)
    vertices(w).auxTree.merge(vertices(v).auxTree)
    vertices(v).parent = vertices(w)
    vertices(w).auxTree.splay()
  }

  def cut(v: Int): Unit = {
    if (vertices(v).isRoot) throw new IllegalArgumentException
    access(v)
    vertices(v).auxTree.cutLeftChild()
    vertices(v).parent = vertices(v)
  }

  def findRoot(v: Int): Int = {
    access(v)
    vertices(v).auxTree.getPathRoot.id
  }

  def getDepth(v: Int): Int = {
    access(v)
    vertices(v).auxTree.getSubtreeSize
  }

  def findRootSlow(v: Int): Int = {
    if (vertices(v).parent == vertices(v)) vertices(v).id
    else findRootSlow(vertices(v).parent.id)
  }

  def access(v: Int): Unit = {
    val vertex = vertices(v)
    vertex.auxTree.splay()
    vertex.auxTree.cutRightChild()
    var root = vertex.auxTree.root
    var pathRoot = root.getPathRoot
    while (!pathRoot.isRoot) {
      val pathParent = pathRoot.parent
      pathParent.auxTree.splay()
      pathParent.auxTree.cutRightChild()
      root = pathParent.auxTree.merge(root)
      pathRoot = root.getPathRoot
    }
    vertex.auxTree.splay()
  }

}

class LinkCutTreeSlow extends LinkCutTree {

  val vertices: ArrayBuffer[Int] = ArrayBuffer.empty[Int]
  var sum = 0
  var t = 0
  def add(): Int = {
    vertices += vertices.size
    vertices.size - 1
  }

  def link(v: Int, w: Int): Unit = {
    if (vertices(v) != v) throw new IllegalArgumentException
    if (findRoot(w) == v) throw new IllegalArgumentException
    vertices(v) = w
  }

  def cut(v: Int): Unit = {
    if (vertices(v) == v) throw new IllegalArgumentException
    vertices(v) = v
  }

  def findRoot(v: Int): Int = {
    if (vertices(v) == v) v
    else findRoot(vertices(v))
  }

  override def findRootSlow(v: Int): Int = findRoot(v)


  override def getDepth(v: Int): Int = if (vertices(v) == v) 0
  else 1 + getDepth(vertices(v))

}

