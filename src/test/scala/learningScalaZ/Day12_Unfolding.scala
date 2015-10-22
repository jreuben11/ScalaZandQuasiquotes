package learningScalaZ

/**
 * Created by joshr on 21/10/2015.
 */

import java.io.{BufferedReader, FileReader}

import org.scalatest.{Matchers, FlatSpec}
//import scala.language.higherKinds
//import scala.language.implicitConversions
//import scala.language.postfixOps

import scalaz._
import Scalaz._
import effect._
import ST._
import IO._
import iteratee._
import Iteratee._

class Day12_Unfolding extends FlatSpec with Matchers {
  "ScalaZ" should "unfold" in {
    DList.unfoldr(10, { (x: Int) => if (x == 0) none else (x, x - 1).some }).toList should be (List(10,9,8,7,6,5,4,3,2,1))
    unfold(10) { (x) => if (x == 0) none else (x, x - 1).some }.toList should be (List(10,9,8,7,6,5,4,3,2,1))
  }

  "ScalaZ ap2" should "turn a Monad into an Applicative, form products, and compose" in {
    Monoid[Int].applicative.ap2(1, 1)(0) should be (2)
    Monoid[List[Int]].applicative.ap2(List(1), List(1))(Nil) should be (List(1, 1))

    Applicative[List].product[Option].point(1) should be ( (List(1), Some(1)) )
    ((List(1), 1.some) |@| (List(1), 1.some)) {_ |+| _} should be ( (List(1,1), Some(2)) )
    ((List(1), 1.success[String]) |@| (List(1), "boom".failure[Int])) {_ |+| _} should be ((List(1, 1),Failure("boom")))

    Applicative[List].compose[Option].point(1) should be (List(some(1)))
    Applicative[Option].compose[List].point(1) should be (some(List(1)))
  }

  "ScalaZ option" should "expand into an if ... Some else None" in {
    def f(x:Int): Option[Int] = {(x > 0) option (x + 1) }
    f(1) should be (2.some)
    f(-1) should be (None)
  }

  "ScalaZ Traverse" should "idiomatically traverse, sequence" in {
    List(1, 2, 3) traverse { x => (x > 0) option (x + 1) } should be (Some(List(2, 3, 4)))
    List(1, 2, 0) traverse { x => (x > 0) option (x + 1) } should be (None)
    Monoid[Int].applicative.traverse(List(1, 2, 3)) {_ + 1} should be (9) //2 + 3 + 4

    List(1.some, 2.some).sequence should be (Some(List(1, 2)))
    List(1.some, 2.some, none).sequence should be (None)

    val validationTree: Tree[Validation[String, Int]] = 1.success[String].node( 2.success[String].leaf, 3.success[String].leaf)
    val failedTree: Tree[Validation[String, Int]] = 1.success[String].node( 2.success[String].leaf, "boom".failure[Int].leaf)


  }


}
class Day15_Arrow extends FlatSpec with Matchers {

}


class Day16_Memoization extends FlatSpec with Matchers {


  "ScalaZ Memo" should "memoize fibanocci" in {


    val y: (Int) => Int = (i: Int) => { i * 2 }


    def slowFib (i: Int):Int = i match {
      case 0 => 0
      case 1 => 1
      case n:Int => slowFib(n - 2) + slowFib(n - 1)
    }
//    val memoizedFib: Int => Int = Memo.mutableHashMapMemo {
//      case 0 => 0
//      case 1 => 1
//      case n => memoizedFib(n - 2) + memoizedFib(n – 1)
//    }
//
//    slowFib(30)
    //memoizedFib(30)

  }

  "ScalaZ ST" should "bloody work" in {
    //def mapM[A, S, B](xs: List[A])(f: A => ST[S, B]): ST[S, List[B]] = Monad[({type [] = ST[S, ]})#].sequence(xs map f)

  }
}

class Day17_IO extends FlatSpec with Matchers {
  "ScalaZ IO" should "work" in {
    val action1 = for {
      _ <- putStrLn("Hello, world!")
    } yield ()
    val action2 = IO {
      val source = scala.io.Source.fromFile("./README.md")
      source.getLines.toStream
    }
    def program: IO[Unit] = for {
      line <- readLn
      _ <- putStrLn(line)
    } yield ()

    (program |+| program).unsafePerformIO

    // IO stream
    val res0 = enumReader[IO](new BufferedReader(new FileReader("./README.md")))
    (head[IoExceptionOr[Char], IO] &= res0).map(_ flatMap {_.toOption}).run.unsafePerformIO()
  }

  "ScalaZ Iteratee" should "work" in {
    def counter[E]: Iteratee[E, Int] = {
      def step(acc: Int)(s: Input[E]): Iteratee[E, Int] =
        s(el = e => cont(step(acc + 1)),
          empty = cont(step(acc)),
          eof = done(acc, eofInput[E])
        )
      cont(step(0))
    }
    (counter[Int] &= enumerate(Stream(1, 2, 3))).run should be (3)
  }

}