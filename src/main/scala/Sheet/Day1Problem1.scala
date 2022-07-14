package Sheet

import scala.annotation.tailrec

// Find the first recurring number (at least one exist)
object Day1Problem1 extends App {

  @tailrec
  def makingLoop(nums: List[Int], slow: Int, fast: Int, count: Int): Int =
    count match {
      // when slow and fast pointer has not met
      case 0 =>
        // when slow and fast pointer meet first time make fast as first element
        if (slow == fast)
          makingLoop(nums, slow, nums.head, count + 1)
        // advance fast pointer twice as fast as slow
        else
          makingLoop(nums, nums(slow), nums(nums(fast)), count)
      // when slow and fast pointer has met once
      case 1 =>
        // when slow and fast pointer meet second time return slow
        if (slow == fast)
          slow
        // advance fast pointer same as slow pointer
        else
          makingLoop(nums, nums(slow), nums(fast), count)
    }

  def findDuplicate(nums: List[Int]): Int = {
    val slow = nums.head
    val fast = nums.head
    makingLoop(nums, nums(slow), nums(nums(fast)), 0)
  }
  val nums = List(2, 5, 9, 6, 9, 3, 8, 9, 7, 1)
  println(findDuplicate(nums))
}
