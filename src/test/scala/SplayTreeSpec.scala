import org.scalatest.FlatSpec
import ru.ifmo.ctddev.advanced_structures.zamyatin.SplayTree

class SplayTreeSpec extends FlatSpec {
  "Splay tree" should "keep elements in the right order #1" in {
    val values = List(0, 1, 2, 3, 4, 5, 6, 7, 8)
    val trees = values.map(SplayTree.create[Int, Int](_, t => t.x))
    var result = trees.head
    for (i <- trees.tail) {
      result = result.merge(i)
    }
    assert(result.toList == values)
  }

  "Splay tree" should "keep elements in the right order #2" in {
    var values = List(0)
    var result = SplayTree.create[Int, Int](0, t => t.x)
    for (i <- 1 until 10) {
      if (i % 2 == 0) {
        result = result.merge(SplayTree.create[Int, Int](i, t => t.x))
        values = values ++ List(i)
      } else {
        result = SplayTree.create[Int, Int](i, t => t.x).merge(result)
        values = List(i) ++ values
      }
    }
    assert(result.toList == values)
  }



}
