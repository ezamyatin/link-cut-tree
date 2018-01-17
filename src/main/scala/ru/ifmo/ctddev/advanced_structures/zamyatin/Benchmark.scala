package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try


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

case class Benchmark(linkTime: Double, cutTime: Double, findRootTime: Double)

object Benchmark extends App {

  def measure[T](f: => T): Double = {
    val s = System.nanoTime()
    val x = f
    val e = System.nanoTime()
    (e - s) * 1d / 1000000
  }

  def perform(linkCutTree: LinkCutTree, n: Int, queries: Seq[(Int, Int, Int)]): Benchmark = {
    SplayTree.reset()
    (0 until n).foreach(_ => linkCutTree.add())
    var (linkTime, cutTime, findTime) = (0d, 0d, 0d)
    var (a, b) = (0, 0)
    for ((t, v1, v2) <- queries) {
      t match {
        case 0 =>
          linkTime += measure(Try(linkCutTree.link(v1, v2)))
        case 1 =>
          cutTime += measure(Try(linkCutTree.cut(v1)))
        case 2 =>
          findTime += measure(Try(linkCutTree.findRoot(v1)))
          a += 1
          b += linkCutTree.getDepth(v1)
      }
    }
    System.err.println(s"Depth: ${b * 1d / a}")
    System.err.println(s"Time: ${SplayTree.time}")
    System.err.println(s"Count: ${SplayTree.count}")
    Benchmark(linkTime, cutTime, findTime)
  }

  def genRandom(n: Int, link: Int, cut: Int, find: Int): Seq[(Int, Int, Int)] = {
    val linkCutTree = new LinkCutTreeFast
    val degree = mutable.TreeSet[(Int, Int)]()
    (0 until n).foreach(i => {
      linkCutTree.add()
      degree.add((0, i))
    })
    val r = (0.1 * n).toInt
    val random = new java.util.Random(39)
    var (linkTime, cutTime, findTime) = (0d, 0d, 0d)
    val res = for (op <- 0 until link + cut + find) yield {
      val t = random.nextInt(link + cut + find)
      t match {
        case _ if t < link =>
          //var (v1, v2) = (random.nextInt(n), random.nextInt(n))
          val v1 = linkCutTree.findRoot(random.nextInt(n))
          val v2 = op % n
          Try(linkCutTree.link(v1, v2))
          (0, v1, v2)
        case _ if link <= t && t < link + cut =>
          val v1 = random.nextInt(n)
          Try(linkCutTree.cut(v1))
          (1, v1, -1)
        case _ =>
          val v1 = random.nextInt(n)
          linkCutTree.findRoot(v1)
          (2, v1, -1)
      }
    }
    System.err.println(linkCutTree.sum * 1d / linkCutTree.t)
    res
  }

  def genRandom1(n: Int, link: Int, cut: Int, find: Int): Seq[(Int, Int, Int)] = {
    val linkCutTree = new LinkCutTreeFast
    (0 until n).foreach(_ => linkCutTree.add())

    val random = new java.util.Random(39)
    var (linkTime, cutTime, findTime) = (0d, 0d, 0d)
    for (op <- 0 until link + cut + find) yield {
      val t = random.nextInt(link + cut + find)
      t match {
        case _ if t < link =>
          var (v1, v2) = (random.nextInt(n), random.nextInt(n))
          v1 = linkCutTree.findRoot(v1)
          Try(linkCutTree.link(v1, v2))
          (0, v1, v2)
        case _ if link <= t && t < link + cut =>
          val v1 = random.nextInt(n)
          Try(linkCutTree.cut(v1))
          (1, v1, -1)
        case _ =>
          val v1 = random.nextInt(n)
          (2, v1, -1)
      }
    }
  }

  /*def worstCase(linkCutTree: LinkCutTree, n: Int): Double = measure {
    val random = new java.util.Random(39)
    (0 until n).foreach(_ => linkCutTree.add())
    for ((i, j) <- (0 until n-1) zip (1 until n)) {
      linkCutTree.link(j, i)
    }
    (0 until n).foreach(_ => linkCutTree.findRoot(n-1))
  }*/

  val ns = List(1 << 15)//(0 until 15).map(math.pow(2, _).toInt)

  if (args(2) == "random") {
    System.out.println("link, cut, find")
    for (n <- ns) {
      val q = (n*n) min 1000000
      val qs = genRandom(n, (0.5 * q).toInt, (0.2 * q).toInt , (0.3 * q).toInt)
      val t = if (args(1).equals("fast")) new LinkCutTreeFast else new LinkCutTreeSlow
      val b = perform(t, n, qs)
      System.out.println(b.linkTime + ", " + b.cutTime + ", " + b.findRootTime)
    }
  } else {
    /*System.out.println("total")
    for (n <- ns) {
      val tree = if (args(1).equals("fast")) new LinkCutTreeFast else new LinkCutTreeSlow
      val b = worstCase(tree, n)
      System.out.println(b)
    }*/
  }

}
