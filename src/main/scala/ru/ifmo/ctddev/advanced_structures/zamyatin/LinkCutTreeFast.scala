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
    vertices(v).auxTree.aggregation._1.id
  }

  def getDepth(v: Int): Int = {
    access(v)
    vertices(v).auxTree.aggregation._2
  }

  def findRootSlow(v: Int): Int = {
    if (vertices(v).parent == vertices(v)) vertices(v).id
    else findRootSlow(vertices(v).parent.id)
  }

  var sum = 0
  var t = 0

  def access(v: Int): Unit = {
    t += 1
    sum += 1
    val vertex = vertices(v)
    vertex.auxTree.splay()
    vertex.auxTree.cutRightChild()
    var root = vertex.auxTree.root
    var pathRoot = root.aggregation._1
    while (!pathRoot.isRoot) {
      sum += 1
      val pathParent = pathRoot.parent
      pathParent.auxTree.splay()
      pathParent.auxTree.cutRightChild()
      root = pathParent.auxTree.merge(root)
      pathRoot = root.aggregation._1
    }
    vertex.auxTree.splay()
  }

}
