package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("the number of elements in a list") {
    val li1 = List('a', 'a', 'b', 'c', 'a', 'b')
    val li2 = times(li1)
    val count = li2.find(_._1 == 'a') match {
      case Some((_, n)) => n
      case None => -1
    }
    
    assert(count == 3)
  }

  test("what if the input for times() is Nil?") {
    assert(times(Nil) == Nil)
  }

  test("list of leaves from frequency") {
    val li = List('a', 'a', 'b', 'c', 'a', 'b')
    val leaves = times(li)

    assert(makeOrderedLeafList(leaves)(1) == Leaf('b', 2))
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine until the leaf list become singleton") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    val folded = until(singleton, combine)(leaflist)
    
    assert(weight(folded(0)) == 7)
  }

  test("creation of code tree from text") {
    val str = "helloworld"
    val set = Set('h', 'e', 'l', 'o', 'w', 'r', 'd')
    val tree = createCodeTree(str.toList)
    assert(chars(tree).toSet == set)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
