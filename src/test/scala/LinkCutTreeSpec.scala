import org.scalatest.FlatSpec
import ru.ifmo.ctddev.advanced_structures.zamyatin.{LinkCutTree, SplayTree}

class LinkCutTreeSpec extends FlatSpec {
  "Link cut tree" should "do correct operations with small test" in {
    val linkCutTree = new LinkCutTree
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

}
