package learningScalaZ

import org.scalatest.{Matchers, FlatSpec}
import scalaz._
import Scalaz._

/**
 * Created by joshr on 14/10/2015.
 */
class Day1_TypeClasses101  extends FlatSpec with Matchers {
  "ScalaZ" should "supply shorthand for scala getorElse, ternary" in {
    1.some | 2 should be (1) // Option getorElse
    (1 > 10)? 1 | 2 should be (2) // ternary
  }

  "ScalaZ Functor" should "use map / lift to transform a function over a Monoid" in {
    // def map[A, B](fa: F[A])(f: A => B): F[B]
    (1, 2, 3) map {_ + 1} should be ((1,2,4)) // Functor map over tuple - maps only last member  - for DI
    val ff: (Int) => Int = ((x: Int) => x + 1) map {_ * 7} // Functor map over function - like andThen
    ff(3) should be (28)
    // def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
    val liftList: (List[Int]) => List[Int] = Functor[List].lift {(_: Int) * 2}
    liftList(List(1,2,3)) should be (List(2,4,6))
  }

  "ScalaZ Functor" should "override the values in a Monoid" in {
    List(1, 2, 3) >| "x"          should be (List("x", "x", "x") )
    List(1, 2, 3) as "x"          should be (List("x", "x", "x") )
    List(1, 2, 3).fpair           should be (List((1,1), (2,2), (3,3)) )
    List(1, 2, 3).strengthL("x")  should be (List(("x",1), ("x",2), ("x",3)) )
    List(1, 2, 3).strengthR("x")  should be (List((1,"x"), (2,"x"), (3,"x")) )
    List(1, 2, 3).void            should be (List((), (), ()) )
  }

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
    (3.some |@| 5.some) {_ + _}  should be (Some(8))
  }



}
