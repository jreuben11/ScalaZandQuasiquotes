package commonAlgos

import scala.annotation.tailrec

/**
  * Created by joshr on 01/03/2016.
  */
object Fibonacci {
  def recursiveFib(i:Int):Int = i match{
    case 0 => 0
    case 1 => 1
    case _ => recursiveFib(i-1) + recursiveFib(i-2)
  }

  lazy val lazyFib: Stream[Int] = 0 #:: 1 #:: lazyFib.zip(lazyFib.tail).map{case (a,b) => a + b}

  @tailrec
  def tailrecursiveFib(x:Int, prev: BigInt = 0, next: BigInt = 1):BigInt = x match {
    case 0 => prev
    case 1 => next
    case _ => tailrecursiveFib(x-1, next, (next + prev))
  }

  val iterativeFib = Iterator.iterate((0,1)){case (a,b) => (b,a+b)}.map(_._1)

  def foldFib(n:Int) = {
    def series(i:BigInt,j:BigInt):Stream[BigInt] = i #:: series(j, i+j)
    series(1,0).take(n).foldLeft(BigInt("0"))(_+_)
  }
}
