package Sheet

import scala.annotation.tailrec

/** Find the number of inversions in an array (using merge sort).
  * Two elements form an inversion if arr(i) > arr(j) and i < j.
  */
object Day2Problem4 extends App {

  @tailrec
  def merge(seq1: List[Int], seq2: List[Int], count: Int, accumulator: List[Int] = List()): (List[Int], Int) =
    (seq1, seq2) match {
      case (Nil, _) => (accumulator ++ seq2, count)
      case (_, Nil) => (accumulator ++ seq1, count)
      case (x :: xs, y :: ys) =>
        if (x < y) merge(xs, seq2, count, accumulator :+ x)
        else {
          merge(seq1, ys, count + (xs.length + 1), accumulator :+ y)
        }
    }

  def mergeSort(seq: List[Int]): (List[Int], Int) = seq match {
    case Nil => (Nil, 0)
    case xs :: Nil => (List(xs), 0)
    case _ =>
      val (left, right) = seq splitAt seq.length / 2
      val sortedLeft = mergeSort(left)._1
      val sortedRight = mergeSort(right)._1
      val inversionCounter = mergeSort(left)._2 + mergeSort(right)._2
      merge(sortedLeft, sortedRight, inversionCounter)
  }
  println(mergeSort(List(8, 4, 2, 1))._2)
}

/** Using counter var */
//  var counter = 0
//  @tailrec
//  def merge(seq1: List[Int], seq2: List[Int], accumulator: List[Int] = List()): List[Int] =
//    (seq1, seq2) match {
//      case (Nil, _) => accumulator ++ seq2
//      case (_, Nil) => accumulator ++ seq1
//      case (x :: xs, y :: ys) =>
//        if (x < y) merge(xs, seq2, accumulator :+ x)
//        else {
//          counter += (xs.length+1)
//          merge(seq1, ys, accumulator :+ y)
//        }
//    }
//
//  def mergeSort(seq: List[Int]): List[Int]= seq match {
//    case Nil => Nil
//    case xs :: Nil => List(xs)
//    case _ =>
//      val (left, right) = seq splitAt seq.length / 2
//
//      merge(mergeSort(left), mergeSort(right))
//  }
//  mergeSort(List(8, 4, 2, 1))
//  println(counter)
