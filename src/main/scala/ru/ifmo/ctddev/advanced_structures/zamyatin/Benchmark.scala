package ru.ifmo.ctddev.advanced_structures.zamyatin

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Try



case class Benchmark(linkTime: Double, cutTime: Double, findRootTime: Double, depth: Double)

object Benchmark extends App {

  def measure[T](f: => T): Double = {
    val s = System.nanoTime()
    val x = f
    val e = System.nanoTime()
    (e - s) * 1d / 1000000
  }

  def perform(linkCutTree: LinkCutTree, n: Int, queries: Seq[(Int, Int, Int)]): Benchmark = {
    System.gc()
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
    Benchmark(linkTime, cutTime, findTime, b * 1d / a)
  }

  def genRandom(n: Int, link: Int, cut: Int, find: Int): Seq[(Int, Int, Int)] = {
    val linkCutTree = new LinkCutTreeFast
    (0 until n).foreach(i => {
      linkCutTree.add()
    })
    val r = (0.1 * n).toInt
    val random = new java.util.Random(39)
    val res = for (op <- 0 until link + cut + find) yield {
      val t = random.nextInt(link + cut + find)
      t match {
        case _ if t < link =>
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
    res
  }

  val ns = (2 until 18).map(1 << _)

  if (args(2) == "random") {
    System.out.println("n, q, depth, link, cut, find, total")
    for (n <- ns) {
      val q = (n.toLong*n) min 1000000L
      val qs = genRandom(n, (0.5 * q).toInt, (0.2 * q).toInt , (0.3 * q).toInt)
      val t = if (args(1).equals("fast")) new LinkCutTreeFast else new LinkCutTreeSlow
      val b = perform(t, n, qs)
      System.out.println(n + ", " + b.linkTime + ", " + b.cutTime + ", " + b.findRootTime + ", " + b.depth + ", " + (b.linkTime + b.cutTime + b.findRootTime))
    }
  }
}
