package learningScalaZ

import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

/**
 * Created by joshr on 14/10/2015.
 */
class Day1_Functor  extends FlatSpec with Matchers {
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
}


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

class Day3_Kinds  extends FlatSpec with Matchers {

  //val x = kind(scala.Option)  // - only in REPL
  trait Test {
    type F[_]
  }
  "ScalaZ Tagged Types" should "ensure that variables match business types" in {
    //type Tagged[U] = { type Tag = U }
    //type @@[T, U] = T with Tagged[U]
    sealed trait KiloGram
    def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
    trait JoulePerKiloGram
    def JoulePerKiloGram[A](a: A): A @@ JoulePerKiloGram = Tag[A, JoulePerKiloGram](a)

    val factor = 299792458.0
    val mass: @@[Double, KiloGram] = KiloGram(20.0)
    def energyR(m: Double @@ KiloGram): Double @@ JoulePerKiloGram = JoulePerKiloGram(Math.pow(factor,2) * Tag.unsubst[Double, Id, KiloGram](m))
    val convertedMass: @@[Double, JoulePerKiloGram] = energyR(mass)
    Tag.unwrap(convertedMass) should be (Math.pow(factor,2) * 20.0)


  }
  "ScalaZ Shows" should "yield a string representation" in {
    List(1, 2, 3).shows should be ("[1,2,3]")
  }

  "ScalaZ Monoids" should "support SemiGroupOp mappend and zero" in {
    List(1, 2, 3) |+| List(4, 5, 6) should be(List(1, 2, 3, 4, 5, 6))
    "one" |+| "two" should be("onetwo")

    Monoid[List[Int]].zero should be(List())
    Monoid[String].zero should be("")
  }
  "ScalaZ Tagged Types" should "should discriminate between alternative type class instances" in {
    Tags.Multiplication(10) |+| Monoid[Int @@ Tags.Multiplication].zero  should be (10)
    10 |+| Monoid[Int].zero should be (10)
    val b: @@[Boolean, Tags.Disjunction] = Tags.Disjunction(true) |+| Tags.Disjunction(false)
    Tag.unwrap(b) should be (true) // b should be (true)
  }
}

class Day4_FunctorLaws  extends FlatSpec with Matchers {
  "ScalaZ Functor Laws" should "uphold identitiy" in {
    val l = List(1, 2, 3)
    l map {identity} should be (l)
  }
}

