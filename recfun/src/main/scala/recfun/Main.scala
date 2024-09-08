package recfun

import scala.annotation.tailrec
import common._

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
    @tailrec def loop(cur: Int, cont: Int => Int): Int =
      if (cur == row)
        if (col == 0 || col == row) 1 else cont(col) + cont(col - 1)
      else {
        val next = (i: Int) =>
          if (i == cur || i == 0) 1 else cont(i) + cont(i - 1)
        loop(cur + 1, next)
      }

    loop(0, _ => 1)
  }

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = ???

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
