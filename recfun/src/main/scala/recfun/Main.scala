package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = if (c == 0) 1 else if (r == c) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_aux(cnt: Int, chars: List[Char]): Boolean = {
      if (cnt < 0)
        false
      else
        chars match {
          case h::t => if (h == '(') balance_aux(cnt + 1, t) else if (h == ')') balance_aux(cnt - 1, t) else balance_aux(cnt, t)
          case Nil => if (cnt == 0) true else false
        }
    }
    balance_aux(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) 0
    else if (money == 0) 1
    else {
      def countChange_aux(money: Int, coins: List[Int], accum: Int): Int = {
        coins match {
          case h::t => countChange_aux(money, t, accum + countChange(money - h, coins))
          case Nil => accum
        }
      }
      countChange_aux(money, coins, 0)
    }
  }
}
