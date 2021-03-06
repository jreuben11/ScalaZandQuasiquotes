package learningScalaZ

import org.scalatest.{FlatSpec, Matchers}

import scala.language.{higherKinds, implicitConversions}
import scalaz.Scalaz._
import scalaz._




class Day2_Applicatives  extends FlatSpec with Matchers {

  "ScalaZ Applicative" should "takes value A and returns F[A]" in {
    // def point[A](a: => A): F[A]
    1.point[List]                 should be ( List(1))
    1.point[Option]               should be ( Some(1))
    1.point[Option] map {_ + 2}   should be ( Some(3))
    1.point[List] map {_ + 2}     should be ( List(3))
  }

  "ScalaZ Applicative <*>" should "extract a function from 1st functor && map it over the 2nd one" in {
    9.some <*> {(_: Int) + 3}.some  should be ( Some(12) )
    1.some <* 2.some                should be ( Some(1) )
    none <* 2.some                  should be ( None )
    1.some *> 2.some                should be ( Some(2) )
    none *> 2.some                  should be (  None ) //???
  }

  "ScalaZ Applicative" should "curry a function over a Monoid" in {
    // without Applicative
    val curried1: List[(Int) => Int] = List(1, 2, 3, 4) map {(_: Int) * (_:Int)}.curried
    curried1 map {_(9)} should be (List(9, 18, 27, 36))
    // with:
    List(1, 2, 3, 4) <*> List({(_: Int) * (_:Int)}.curried(9))  should be (List(9, 18, 27, 36))
    3.some <*> { 9.some <*> {(_: Int) + (_: Int)}.curried.some } should be (Some(12))


    // apply 3 functions to 3 list members
    List(1, 2, 3) <*> List(
      (_: Int) * 0,
      (_: Int) + 100,
      (x: Int) => x * x
    ) should be ( List(
      0, 0, 0,
      101, 102, 103,
      1, 4, 9
    ))
    // apply 2 functions to a 2 x 2 cross-product
    List(3, 4) <*> { List(1, 2) <*> List(
      {(_: Int) + (_: Int)}.curried,
      {(_: Int) * (_: Int)}.curried
    )} should be ( List(3+1, 4+1, 3+2, 4+2, 3*1, 4*1, 3*2, 4*2))

    // apply a 3x3 + cross-product:
    (List("ha", "heh", "hmm") |@| List("?", "!", ".")) {_ + _} should be (
      List("ha?", "ha!", "ha.", "heh?", "heh!", "heh.", "hmm?", "hmm!", "hmm."))
  }

  "ScalaZ Applicative" should "extract values from Monoids and apply a function to them" in {
    ^(3.some, 5.some) {_ + _} should be (Some(8))
    ^(3.some, None) {_ + _}   should be (None)
    ^(List(1, 2, 3), List(10, 100, 100)) {_ * _} should be (List(10, 100, 100, 20, 200, 200, 30, 300, 300) )
    (3.some |@| 5.some) {_ + _}  should be (Some(8))
  }

  "ScalaZ Applicative" should "not really sure" in {
    val f = Apply[Option].lift2((_: Int) :: (_: List[Int]))
    f(3.some, List(4).some) should be  (Some(List(3, 4)))

    def sequenceA[F[_]: Applicative, A](list: List[F[A]]): F[List[A]]  = list match {
      case Nil => (Nil: List[A]).point[F]
      case x :: xs => (x |@| sequenceA(xs)) {_ :: _}
    }
    sequenceA(List(1.some, 2.some))       should be (Some(List(1, 2)))
    sequenceA(List(3.some, none, 1.some)) should be (None)
    sequenceA(List(List(1, 2, 3), List(4, 5, 6))) should be (
      List(
        List(1, 4),
        List(1, 5),
        List(1, 6),
        List(2, 4),
        List(2, 5),
        List(2, 6),
        List(3, 4),
        List(3, 5),
        List(3, 6)
      )
    )

  }
}





