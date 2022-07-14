package Sheet

import scala.annotation.tailrec

// Sort array of 0,1,2 without extra space (Dutch National Flag Algorithm)
object Day1Problem2 extends App {

  @tailrec
  def sortArray(arr: Array[Int], lo: Int, hi: Int, mid: Int): Unit =
    if (mid <= hi)
      arr(mid) match {
        // if arr(mid) is 0 then swap lo with mid and increment lo and mid by 1
        case 0 =>
          val temp = arr(lo)
          arr(lo) = arr(mid)
          arr(mid) = temp
          sortArray(arr, lo + 1, hi, mid + 1)
        // if arr(mid) is 1 then increment mid by 1
        case 1 =>
          sortArray(arr, lo, hi, mid + 1)
        // if arr(mid) is 2 then swap hi with mid and decrement hi by 1
        case 2 =>
          val temp = arr(hi)
          arr(hi) = arr(mid)
          arr(mid) = temp
          sortArray(arr, lo, hi - 1, mid)
      }

  val arr = Array(0, 1, 1, 0, 1, 2, 1, 2, 0, 0, 0, 1)
  sortArray(arr, 0, arr.length - 1, 0)
  println(arr.toList)
}
