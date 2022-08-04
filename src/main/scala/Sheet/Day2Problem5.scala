package Sheet

import scala.annotation.tailrec

/** In a given array of prices of a particular stock, find the maximum profit by buying and selling.
  */
object Day2Problem5 extends App {

  @tailrec
  def buySellStock(prices: List[Int], profit: Int, minimal: Int): Int =
    if (prices.isEmpty)
      profit
    else {
      if (prices.head < minimal)
        buySellStock(prices.tail, profit, prices.head)
      else
        buySellStock(prices.tail, profit.max(prices.head - minimal), minimal)
    }
  println(buySellStock(List(7, 1, 5, 3, 6, 4), 0, Int.MaxValue))
}
