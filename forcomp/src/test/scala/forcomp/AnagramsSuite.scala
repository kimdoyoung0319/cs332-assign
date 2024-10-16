package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("mergeOccurences: List(('a', 3), ('b', 4)) with List (('a', 2))") {
    val a = List(('a', 3), ('b', 4))
    val b = List(('a', 2))

    assert(mergeOccurrences(a, b) === List(('a', 5), ('b', 4)))
  }

  test("mergeOccurences: List(('a', 3), ('b', 4)) with List (('c', 2))") {
    val a = List(('a', 3), ('b', 4))
    val b = List(('c', 2))

    assert(mergeOccurrences(a, b) === List(('a', 3), ('b', 4), ('c', 2)))
  }

  test("wordOccurrences: abcd") {
    assert(
      wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1))
    )
  }

  test("wordOccurrences: Robert") {
    assert(
      wordOccurrences("Robert") === List(
        ('b', 1),
        ('e', 1),
        ('o', 1),
        ('r', 2),
        ('t', 1)
      )
    )
  }

  test("sentenceOccurrences: abcd e") {
    assert(
      sentenceOccurrences(List("abcd", "e")) === List(
        ('a', 1),
        ('b', 1),
        ('c', 1),
        ('d', 1),
        ('e', 1)
      )
    )
  }

  test("sentenceOccurrences: helLo wOrld") {
    assert(
      sentenceOccurrences(List("helLo", "wOrld")) === List(
        ('d', 1),
        ('e', 1),
        ('h', 1),
        ('l', 3),
        ('o', 2),
        ('r', 1),
        ('w', 1)
      )
    )
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(
      dictionaryByOccurrences
        .get(List(('a', 1), ('e', 1), ('t', 1)))
        .map(_.toSet) === Some(Set("ate", "eat", "tea"))
    )
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(
      wordAnagrams("player").toSet === Set(
        "parley",
        "pearly",
        "player",
        "replay"
      )
    )
  }

  test("power set: ('a', 4)") {
    assert(
      powerOccurrence(('a', 4)) === List(
        List(),
        List(('a', 1)),
        List(('a', 2)),
        List(('a', 3)),
        List(('a', 4))
      )
    )
  }

  test("cartesian: power set of ('a', 2) and power set of ('b', 1))") {
    val a = powerOccurrence(('a', 2))
    val b = powerOccurrence(('b', 1))
    val expected = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2)),
      List(('a', 2), ('b', 1))
    )
    assert(cartesian(a, b).toSet === expected.toSet)
  }

  test("combinations: subset of List(('a', 1), ('b', 2))") {
    val occurrences = List(('a', 1), ('b', 2))
    val expected = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('b', 2))
    )
    assert(combinations(occurrences).toSet === expected.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: List(('a', 2), ('b', 3)) - List(('a', 1), ('b', 1))") {
    val a = List(('a', 2), ('b', 3))
    val b = List(('a', 1), ('b', 1))
    val expected = List(('a', 1), ('b', 2))
    assert(subtract(a, b) === expected)
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
