package learningScalaZ

import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/**
 * Created by joshr on 17/10/2015.
 */
class Day5_Monads extends FlatSpec with Matchers {
  // trait Monad[F[_]] extends Applicative[F] with Bind[F] { self => ////
  // def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  // import Liskov.<~<
  "ScalaZ Monads" should "FlatMap, Point" in {
    3.some flatMap { x => (x + 1).some } should be (4.some)
    (none: Option[Int]) flatMap { x => (x + 1).some } should be (None)
    Monad[Option].point("WHAT") should be (Some("WHAT"))

    (3.some >>= { x => "!".some >>= { y => (x.shows + y).some } }) should be (Some("3!"))
    // same:
    for {
      x <- 3.some
      y <- "!".some
    } yield Some(x.shows + y) should be (Some("3!"))
  }

  "ScalaZ Monads" should "FlatMap chaining - intermediate values can fail, but the calculation can keep going" in {
    type Birds = Int
    case class Pole(left: Birds, right: Birds) {
      def landLeft(n: Birds): Option[Pole] =
        if (math.abs((left + n) - right) < 4) copy(left = left + n).some
        else none
      def landRight(n: Birds): Option[Pole] =
        if (math.abs(left - (right + n)) < 4) copy(right = right + n).some
        else none
      def banana: Option[Pole] = none
    }
    Pole(0, 0).landLeft(1) should be (Some(Pole(1,0)))
    Pole(0, 0).landLeft(4) should be (None)
    Pole(3, 0).landLeft(1) should be (None)
    Monad[Option].point(Pole(0, 0)) flatMap {_.landRight(2)} flatMap {_.landLeft(2)} flatMap {_.landRight(2)} should be (Some(Pole(2,4)))
    (Monad[Option].point(Pole(0, 0)) >>= {_.landRight(2)} >>= {_.landLeft(2)} >>= {_.landRight(2)}) should be (Some(Pole(2,4)))
    (Monad[Option].point(Pole(0, 0)) >>= {_.landRight(4)} >>= {_.landLeft(2)} >>= {_.landRight(2)}) should be (None)
    (Monad[Option].point(Pole(0, 0)) >>= {_.landLeft(1)} >>= {_.banana} >>= {_.landRight(1)}) should be (None)
    for {
      start <- Monad[Option].point(Pole(0, 0))
      first <- start.landLeft(2)
      second <- first.landRight(2)
      third <- second.landLeft(1)
    } yield Some(third) should be (Some(Pole(3,2)))

    def justHead(s:String): Option[Char] =
      for {
        (x :: xs) <- s.toList.some
      } yield x
    justHead("") should be (None)
    justHead("hello") should be (Some('h'))
  }

  "ScalaZ Monads" should "List Applicatives & Monads" in {
    // applicative apply
    ^(List(1, 2, 3), List(10, 100, 100)) {_ * _} should be (List(10, 100, 100, 20, 200, 200, 30, 300, 300) )
    (List(3, 4, 5) >>= {x => List(x, -x)}) should be (List(3, -3, 4, -4, 5, -5))

    (for {
      n <- List(1, 2)
      ch <- List('a', 'b')
    } yield (n, ch) )  should be (List((1,'a'), (1,'b'), (2,'a'), (2,'b')) )
    (for {
      x <- 1 |-> 50 if x.shows contains '7'
    } yield x) should be (List(7, 17, 27, 37, 47))

    // Plus <+> operator to append two containers:
    List(1, 2, 3) <+> List(4, 5, 6)should be (List(1, 2, 3, 4, 5, 6))
    (1 |-> 50) filter { x => x.shows contains '7' } should be (List(7, 17, 27, 37, 47))

  }
  "ScalaZ Monads" should "Laws" in {
    // \> Id Op
    (Monad[Option].point(3) >>= { x => (x + 100000).some }) should be (3 |> { x => (x + 100000).some })
    ("move on up".some flatMap {Monad[Option].point(_)}) should be ("move on up".some)

  }
}
