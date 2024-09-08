package recfun
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
    if (col == row || col == 0) 1
    else pascal(col, row - 1) + pascal(col - 1, row - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    def loop(accum: Boolean, count: Int, remaining: List[Char]): Boolean = {
      if (remaining.isEmpty)
        accum
      else {
        remaining.head match {
          case '(' => loop(false, count + 1, remaining.tail)
          case ')' => loop(count == 1, count - 1, remaining.tail)
          case _   => loop(accum, count, remaining.tail)
        }
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
