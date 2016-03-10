package commonAlgos

import scala.annotation.tailrec

/**
  * Created by joshr on 10/03/2016.
  */
object Sort {

  def countSort(input: List[Int], min: Int, max: Int): List[Int] =
    input.foldLeft(Array.fill(max - min + 1)(0)) { (arr, n) =>
      arr(n - min) += 1
      arr
    }.zipWithIndex.foldLeft(List[Int]()) {
      case (lst, (cnt, ndx)) => List.fill(cnt)(ndx + min) ::: lst
    }.reverse


  def insertSort[X](list: List[X])(implicit ord: Ordering[X]) = {
    def insert(list: List[X], value: X) = list.span(x => ord.lt(x, value)) match {
      case (lower, upper) => lower ::: value :: upper
    }
    list.foldLeft(List.empty[X])(insert)
  }

  def selectionSort[T <% Ordered[T]](list: List[T]): List[T] = {
    def remove(e: T, list: List[T]): List[T] =
      list match {
        case Nil => Nil
        case x :: xs if x == e => xs
        case x :: xs => x :: remove(e, xs)
      }

    list match {
      case Nil => Nil
      case _ =>
        val min = list.min
        min :: selectionSort(remove(min, list))
    }
  }


  def heapSort[T](a: Array[T])(implicit ord: Ordering[T]) {
    import ord._

    val indexOrdering = Ordering by a.apply

    def numberOfLeaves(heapSize: Int) = (heapSize + 1) / 2

    def children(i: Int, heapSize: Int) = {
      val leftChild = i * 2 + 1
      leftChild to leftChild + 1 takeWhile (_ < heapSize)
    }

    def swap(i: Int, j: Int) = {
      val tmp = a(i)
      a(i) = a(j)
      a(j) = tmp
    }

    // Maintain partial ordering by bubbling down elements
    @tailrec
    def siftDown(i: Int, heapSize: Int) {
      val childrenOfI = children(i, heapSize)
      if (childrenOfI nonEmpty) {
        val biggestChild = childrenOfI max indexOrdering
        if (a(i) < a(biggestChild)) {
          swap(i, biggestChild)
          siftDown(biggestChild, heapSize)
        }
      }
    }

    // Prepare heap by sifting down all non-leaf elements
    for (i <- a.indices.reverse drop numberOfLeaves(a.length)) siftDown(i, a.size)

    // Sort from the end of the array forward, by swapping the highest element,
    // which is always the top of the heap, to the end of the unsorted array
    for (i <- a.indices.reverse) {
      swap(0, i)
      siftDown(0, i)
    }
  }

  def mergeSort(input: List[Int]) = {
    def merge(left: List[Int], right: List[Int]): Stream[Int] = (left, right) match {
      case (x :: xs, y :: ys) if x <= y => x #:: merge(xs, right)
      case (x :: xs, y :: ys) => y #:: merge(left, ys)
      case _ => if (left.isEmpty) right.toStream else left.toStream
    }
    def sort(input: List[Int], length: Int): List[Int] = input match {
      case Nil | List(_) => input
      case _ =>
        val middle = length / 2
        val (left, right) = input splitAt middle
        merge(sort(left, middle), sort(right, middle + length % 2)).toList
    }
    sort(input, input.length)
  }

  def bubbleSort(xt: List[Int]) = {
    @tailrec
    def bubble(xs: List[Int], rest: List[Int], sorted: List[Int]): List[Int] = xs match {
      case x :: Nil =>
        if (rest.isEmpty) x :: sorted
        else bubble(rest, Nil, x :: sorted)
      case a :: b :: xs =>
        if (a > b) bubble(a :: xs, b :: rest, sorted)
        else       bubble(b :: xs, a :: rest, sorted)
    }
    bubble(xt, Nil, Nil)
  }
}
