package Sheet

/** Rotate nxn 2D matrix clockwise
  */
object Day2Problem6 extends App {

  def rotateClockwise(arr: Array[Array[Int]]): Unit = {
    for {
      row <- arr.indices
      col <- row until arr.length
    } {
      val temp = arr(row)(col)
      arr(row)(col) = arr(col)(row)
      arr(col)(row) = temp
    }
    arr.indices.foreach { index =>
      for (col <- 0 to arr.length / 2) {
        val temp = arr(index)(col)
        arr(index)(col) = arr(index)(arr.length - 1 - col)
        arr(index)(arr.length - 1 - col) = temp
      }
    }
    arr.foreach { row =>
      println(row.mkString(", "))
    }
  }
  val arr = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  rotateClockwise(arr)
}
