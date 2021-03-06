import org.scalatest.FlatSpec
import ru.ifmo.ctddev.advanced_structures.zamyatin.{LinkCutTreeFast, LinkCutTreeSlow, SplayTree}

import scala.util.{Random, Try}

class LinkCutTreeSpec extends FlatSpec {
  "Link cut tree" should "do correct operations with small test" in {
    val linkCutTree = new LinkCutTreeFast
    val (v1, v2, v3) = (linkCutTree.add(), linkCutTree.add(), linkCutTree.add())
    assert(linkCutTree.findRoot(v1) == v1 &&
      linkCutTree.findRoot(v2) == v2 &&
      linkCutTree.findRoot(v3) == v3)

    linkCutTree.link(v1, v2)
    assert(linkCutTree.findRoot(v1) == v2)
    linkCutTree.link(v2, v3)
    assert(linkCutTree.findRoot(v1) == v3 && linkCutTree.findRoot(v2) == v3)
    linkCutTree.cut(v1)
    assert(linkCutTree.findRoot(v1) == v1)
    linkCutTree.link(v3, v1)
    assert(linkCutTree.findRoot(v2) == v1 && linkCutTree.findRoot(v3) == v1)
    linkCutTree.cut(v3)
    linkCutTree.link(v1, v3)
    assert(linkCutTree.findRoot(v2) == v3)
    assert(linkCutTree.findRoot(v1) == v3)
  }

  "Link cut tree" should "do correct operations with small hand test #1" in {
    val linkCutTree = new LinkCutTreeFast
    val vs = (0 until 7).map(_ => linkCutTree.add()).toArray
    linkCutTree.link(3, 1)
    linkCutTree.link(2, 5)
    linkCutTree.link(5, 3)
    linkCutTree.link(1, 6)
    linkCutTree.link(4, 3)
    linkCutTree.cut(1)
    assert(linkCutTree.findRoot(2) == 1)
  }

  "Link cut tree" should "do correct operations with small hand test #2" in {
    val linkCutTree = new LinkCutTreeFast
    val vs = (0 until 5).map(_ => linkCutTree.add()).toArray
    linkCutTree.link(1, 0)
    linkCutTree.link(2, 1)
    linkCutTree.link(3, 2)
    linkCutTree.link(4, 3)
    linkCutTree.cut(1)
    linkCutTree.findRoot(0)
    linkCutTree.findRoot(1)
    linkCutTree.findRoot(2)
    linkCutTree.findRoot(3)
    assert(linkCutTree.findRoot(4) == 1)
  }



  def doAllLinkAllCutQueries(n: Int, q: Int, seed: Int) = {
    val random = new java.util.Random(seed)
    val linkCutTree = new LinkCutTreeFast
    val linkCutTreeSlow = new LinkCutTreeSlow

    def check = for (i <- 0 until n) {
      assert(linkCutTree.findRoot(i) == linkCutTreeSlow.findRoot(i))
    }

    (0 until n).foreach(_ => {linkCutTree.add(); linkCutTreeSlow.add()})
    for (_ <- 0 until q) {
      var (v1, v2) = (random.nextInt(n), random.nextInt(n))
      v1 = linkCutTreeSlow.findRoot(v1)
      Try(linkCutTree.link(v1, v2))
      Try(linkCutTreeSlow.link(v1, v2))
      check
    }
    for (v <- 0 until n) {
      Try(linkCutTree.cut(v))
      Try(linkCutTreeSlow.cut(v))
      check
    }
  }

  def doLinkCutQueries(n: Int, q: Int, seed: Int) = {
    val random = new java.util.Random(seed)

    val linkCutTree = new LinkCutTreeFast
    val linkCutTreeSlow = new LinkCutTreeSlow

    def check = for (i <- 0 until n) {
      assert(linkCutTree.findRoot(i) == linkCutTreeSlow.findRoot(i))
    }

    (0 until n).foreach(_ => {linkCutTree.add(); linkCutTreeSlow.add()})

    for (i <- 0 until q) {
      var (v1, v2) = (random.nextInt(n), random.nextInt(n))
      v1 = linkCutTreeSlow.findRoot(v1)
      if (random.nextBoolean()) {
        Try(linkCutTree.link(v1, v2))
        Try(linkCutTreeSlow.link(v1, v2))
      }
      else {
        Try(linkCutTree.cut(v2))
        Try(linkCutTreeSlow.cut(v2))
      }
      check
    }
  }


  "Link cut tree" should "do correct link and cut operations with small random tests #1" in {
    doAllLinkAllCutQueries(5, 4, 281)
  }

  "Link cut tree" should "do correct link and cut operations with small random tests #2" in {
    doAllLinkAllCutQueries(10, 100, 239)
  }

  "Link cut tree" should "do correct link and cut operations with random tests #1" in {
    doLinkCutQueries(10, 1000, 39)
  }

  "Link cut tree" should "do correct link and cut operations with random tests #2" in {
    doLinkCutQueries(100, 10000, 39)
  }

}
