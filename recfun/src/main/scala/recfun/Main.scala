package recfun

import common._
import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  def pascal(col: Int, row: Int): Int = {

    @tailrec
    def loop(index: Int, previous: Array[Int]): Int = {
      val current = new Array[Int](index + 1)
      for (i <- 0 to index)
        current(i) =
          if (i == 0 || i == index) 1 else previous(i) + previous(i - 1)
      if (index == row) current(col) else loop(index + 1, current)
    }

    if (row == 0) 1 else loop(1, Array(1))
  }

  def balance(chars: List[Char]): Boolean = {

    def loop(accum: Boolean, count: Int, remaining: List[Char]): Boolean = {
      remaining match {
        case Nil         => accum
        case '(' :: tail => loop(false, count + 1, tail)
        case ')' :: tail => loop(count == 1, count - 1, tail)
        case _ :: tail   => loop(accum, count, tail)
      }
    }

    loop(true, 0, chars)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted = coins.sorted.reverse

    def loop(remaining: Int, coins: List[Int]): Int =
      (remaining, coins) match {
        case (0, _)   => 1
        case (_, Nil) => 0
        case (_, head :: tail) =>
          if (remaining < head) loop(remaining, tail)
          else loop(remaining - head, coins) + loop(remaining, tail)
      }

    loop(money, sorted)
  }
}
