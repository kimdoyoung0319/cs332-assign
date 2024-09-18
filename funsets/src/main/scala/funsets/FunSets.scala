package funsets

import common._
import scala.annotation.tailrec

/** 2. Purely Functional Sets.
  */
object FunSets {

  /** We represent a set by its characteristic function, i.e. its `contains`
    * predicate.
    */
  type Set = Int => Boolean

  /** Indicates whether a set contains a given element.
    */
  def contains(set: Set, elem: Int): Boolean = set(elem)

  /** Returns the set of the one given element.
    */
  def singletonSet(elem: Int): Set = (_ == elem)

  /** Returns the union of the two given sets, the sets of all elements that are
    * in either `first` or `second`.
    */
  def union(first: Set, second: Set): Set =
    elem => contains(first, elem) || contains(second, elem)

  /** Returns the intersection of the two given sets, the set of all elements
    * that are both in `first` and `second`.
    */
  def intersect(first: Set, second: Set): Set =
    elem => contains(first, elem) && contains(second, elem)

  /** Returns the difference of the two given sets, the set of all elements of
    * `first` that are not in `second`.
    */
  def diff(first: Set, second: Set): Set =
    elem => contains(first, elem) && !contains(second, elem)

  /** Returns the subset of `set` for which `predicate` holds.
    */
  def filter(set: Set, predicate: Int => Boolean): Set =
    elem => contains(set, elem) && predicate(elem)

  /** The bounds for `forall` and `exists` are +/- 1000.
    */
  val UpperBound = 1000
  val LowerBound = -1000

  /** Returns whether all bounded integers within `set` satisfy `predicate`.
    */
  def forall(set: Set, predicate: Int => Boolean): Boolean = {

    @tailrec
    def loop(index: Int): Boolean = {
      if (index > UpperBound)
        true
      else if (contains(set, index) && !predicate(index))
        false
      else
        loop(index + 1)
    }

    loop(LowerBound)
  }

  /** Returns whether there exists a bounded integer within `set` that satisfies
    * `predicate`.
    */
  def exists(set: Set, predicate: Int => Boolean): Boolean = {

    @tailrec
    def loop(index: Int): Boolean = {
      if (index > UpperBound)
        false
      else if (contains(set, index) && predicate(index))
        true
      else
        loop(index + 1)
    }

    loop(LowerBound)
  }

  /** Returns a set transformed by applying `mapping` to each element of `set`.
    */
  def map(set: Set, mapping: Int => Int): Set =
    (elem: Int) => exists(set, mapping(_) == elem)

  /** Displays the contents of a set
    */
  def toString(set: Set): String = {
    val elemStrings =
      for (index <- LowerBound to UpperBound if contains(set, index))
        yield index
    elemStrings.mkString("{", ",", "}")
  }

  /** Prints the contents of a set on the console.
    */
  def printSet(s: Set) {
    println(toString(s))
  }
}
