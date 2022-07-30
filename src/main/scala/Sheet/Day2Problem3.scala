package Sheet

// find next permutation in dictionary order
object Day2Problem3 extends App {

  def findNextPermutation(arr: Array[Int]): Array[Int] = {

    /**    5
      *   3  4
      *  1    2
      *
      *  since, arr(1) < arr(2) therefore ind1 = 1
      */
    val ind1 = (for (i <- arr.length - 2 to 0 by -1 if arr(i) < arr(i + 1)) yield i).headOption.getOrElse(-1)

    ind1 match {
      // if ind1 is -1 that means there is no element before the largest element
      // then just reverse the array
      case -1 => arr.reverse
      case _ =>
        /**    5
          *   3  4
          *  1    2
          *
          *  here, ind1 = 1
          *  since, arr(3) > arr(ind1) therefore ind2 = 3
          */
        val ind2 = (for (i <- arr.length - 1 to 0 by -1 if arr(i) > arr(ind1)) yield i).head

        // swap arr(ind1) with arr(ind2)
        val temp = arr(ind1)
        arr(ind1) = arr(ind2)
        arr(ind2) = temp

        // reverse the elements after ind1
        val start: Array[Int] = arr.slice(0, ind1 + 1)
        val last: Array[Int] = arr.slice(ind1 + 1, arr.length).reverse

        start.concat(last)

      /* reverse element by indexing if do not want to use extra space.
        (0 to (arr.length - ind1 - 1) / 2) foreach { ind =>
          val temp = arr(ind1 + 1 + ind)
          arr(ind1 + 1 + ind) = arr(arr.length - 1 - ind)
          arr(arr.length - 1 - ind) = temp
          println(arr.mkString(" ,"))
        }
        arr
       */
    }
  }
  val arr: Array[Int] = Array(1, 3, 5, 4, 2)
  println(findNextPermutation(arr).mkString(","))
}
