package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.collection.mutable.ArrayBuffer

class LinkCutTree {

  class Vertex(var id: Int) {
    var parent: Vertex = this
    var auxTree: SplayTree[Vertex, Vertex] = SplayTree.create(this, v => v.getLeft.fold(v.x)(_.aggregation))

    def isRoot = parent == this
    override def toString: String = s"Vertex($id,${parent.id})"
  }

  private var vertices: ArrayBuffer[Vertex] = ArrayBuffer.empty

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
  }

  def cut(v: Int): Unit = {
    if (vertices(v).isRoot) throw new IllegalArgumentException
    access(v)
    vertices(v).auxTree.cutLeftChild()
    vertices(v).parent = vertices(v)
  }

  def findRoot(v: Int): Int = {
    access(v)
    vertices(v).auxTree.aggregation.id
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
    var pathRoot = root.aggregation
    while (!pathRoot.isRoot) {
      val pathParent = pathRoot.parent
      pathParent.auxTree.splay()
      pathParent.auxTree.cutRightChild()
      root = pathParent.auxTree.merge(root)
      pathRoot = root.aggregation
    }
    vertex.auxTree.splay()
  }

}
