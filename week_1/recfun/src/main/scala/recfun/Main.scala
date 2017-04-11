package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    def loop(col: Int, row: Int): Int = {
      if (col <= 0 || col >= row) 1
      else if (col == 1 || col == row) row
      else if (row <= 1) 1
      else loop(col - 1, row - 1) + loop(col, row - 1)
    }
    loop(c, r)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(acc: Int, chars: List[Char]): Boolean = {
      if (acc < 0) false
      else if (chars.isEmpty) acc == 0
      else {
        val value =
          if (chars.head == '(') 1
          else if (chars.head == ')') -1
          else 0
        loop(acc + value, chars.tail)
      }
    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(m: Int, coins: List[Int]): Int = {
      if (m == 0) 1
      else if (m < 0 || coins.isEmpty) 0
      else loop(m - coins.head, coins) + loop(m, coins.tail)
    }

    if (coins.isEmpty) 0
    else loop(money - coins.head, coins) + loop(money, coins.tail)
  }
}
