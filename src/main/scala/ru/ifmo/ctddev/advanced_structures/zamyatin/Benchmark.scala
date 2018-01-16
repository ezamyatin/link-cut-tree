package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


class LinkCutTreeSlow extends LinkCutTree {

  val vertices: ArrayBuffer[Int] = ArrayBuffer.empty[Int]

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
}

case class Benchmark(linkTime: Double, cutTime: Double, findRootTime: Double)

object Benchmark extends App {

  def measure[T](f: => T): Double = {
    val s = System.nanoTime()
    val x = f
    val e = System.nanoTime()
    (e - s) * 1d / 1000000
  }

  def randomBenchmark(linkCutTree: LinkCutTree, n: Int, q: Int): Benchmark = {
    val random = new java.util.Random(39)
    (0 until n).foreach(_ => linkCutTree.add())
    var (linkTime, cutTime, findTime) = (0d, 0d, 0d)
    for (op <- 0 until q) {
      val t = random.nextInt(3)
      t match {
        case 0 =>
          val (v1, v2) = (random.nextInt(n), random.nextInt(n))
          linkTime += measure(Try(linkCutTree.link(v1, v2)))
        case 1 =>
          val v1 = random.nextInt(n)
          cutTime += measure(Try(linkCutTree.cut(v1)))
        case 2 =>
          val v1 = random.nextInt(n)
          findTime += measure(Try(linkCutTree.findRoot(v1)))
      }
    }
    Benchmark(linkTime, cutTime, findTime)
  }

  def worstCase(linkCutTree: LinkCutTree, n: Int): Double = measure {
    val random = new java.util.Random(39)
    (0 until n).foreach(_ => linkCutTree.add())
    for ((i, j) <- (0 until n-1) zip (1 until n)) {
      linkCutTree.link(j, i)
    }
    (0 until n).foreach(_ => linkCutTree.findRoot(n-1))
  }

  val ns = (0 until 15).map(math.pow(2, _).toInt)
  if (args(2) == "random") {
    System.out.println("link, cut, find")
    for (n <- ns) {
      val tree = if(args(1).equals("fast")) new LinkCutTreeFast else new LinkCutTreeSlow
      val q = (n * n) min 1000000
      val b = randomBenchmark(tree, n, q)
      System.out.println(b.linkTime + ", " + b.cutTime + ", " + b.findRootTime)
    }
  } else {
    System.out.println("total")
    for (n <- ns) {
      val tree = if (args(1).equals("fast")) new LinkCutTreeFast else new LinkCutTreeSlow
      val b = worstCase(tree, n)
      System.out.println(b)
    }
  }

}
