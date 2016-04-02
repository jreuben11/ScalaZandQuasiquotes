package learningCats

import cats.std.double
import cats.{Apply, Applicative}
import org.scalatest.{Matchers, FlatSpec}

import cats.data.{Validated, Xor}
import cats.syntax.apply
import cats.syntax.apply._  // For |@| syntax

import cats._
import cats.std.all._
import cats.syntax.all._
// import cats.implicits._

import cats.data.Xor.{left,right}
import cats.data.Validated.{Invalid, invalid, valid}
/**
  * Created by joshr on 01/04/2016.
  */
class catsDataTest extends FlatSpec with Matchers{
  "Xor" should "behave as a right-biased Either" in {

    type Result[A] = String Xor A
    val l: Result[Int] = left("Failed")
    val r: Result[Int] = right(1)

    // Nothing happens when we map a left
    l.map(x => x + 1) shouldBe left("Failed")
    l.map(x => x + 1).toOption shouldBe None
    // The right is transformed
    r.map(x => x + 1).toOption shouldBe Some(2)
    r.map(x => x + 1) shouldBe right(2)
  }

  "Xor and Validated" should "behave differently on flatmap" in {
    type Error = List[String]
    type XorR  = Xor[Error, Int]
    type ValidatedR = Validated[Error, Int]

    val x1: XorR = right(1)
    val x2: XorR = left(List("Stops here"))
    val x3: XorR = left(List("This will be ignored"))

    val v1: ValidatedR = valid(1)
    val v2: ValidatedR = invalid(List("Accumulates this"))
    val v3: ValidatedR = invalid(List("And this"))

    // Stops as soon as we encounter an error
    val x: Xor[Error, Int] =
      for {
        x <- x1
        y <- x2
        z <- x3
      } yield x + y + z
    x shouldBe left(List("Stops here"))

    // Accumulates all the errors
     val v = (v1 |@| v2 |@| v3).map { _ + _ + _ }

    v shouldBe Invalid(List("Accumulates this", "And this"))
    println(v)


  }

  "Applicative.pure" should "returns a value in the context of the functor" in {
    Applicative[Option].pure(1) shouldBe Some(1)
    Applicative[List].pure(1) shouldBe List(1)
    (Applicative[List] compose Applicative[Option]).pure(1) shouldBe List(Some(1))
  }



  "Functor" should "support map, lift, fproduct" in {
    val len: String => Int = _.length
    Functor[Option].map(Some("adsf"))(len) shouldBe Some(4)
    Functor[Option].map(None)(len) shouldBe None
    Functor[List].map(List("qwer", "adsfg"))(len) shouldBe List(4, 5)

    val lenOption: Option[String] => Option[Int] = Functor[Option].lift(len)
    lenOption(Some("abcd")) shouldBe Some(4)
    lenOption(None) shouldBe None

    val source = List("a", "aa", "b", "ccccc")
    Functor[List].fproduct(source)(len).toMap shouldBe Map("a" -> 1, "aa" -> 2, "b" -> 1, "ccccc" -> 5)

    val listOpt = Functor[List] compose Functor[Option]
    listOpt.map(List(Some(1), None, Some(3)))(_ + 1) shouldBe List(Some(2), None, Some(4))
  }

  "Apply" should "extend Functor and support map / ap" in {
    val intToString: Int => String = _.toString
    val double: Int => Int = _ * 2
    Apply[Option].map(Some(1))(intToString) shouldBe Some("1")
    Apply[Option].map(Some(1))(double) shouldBe Some(2)

    Apply[Option].ap(Some(intToString))(Some(1)) shouldBe Some("1")
    Apply[Option].ap(Some(double))(Some(1)) shouldBe Some(2)
  }

  "Monad" should "extend Apply with flatten, flatmap, pure, ifM" in {
    Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4)) shouldBe List(1, 2, 3, 4, 1, 2)
  }

  "SemiGroup" should "associative combine" in {
    Semigroup[Int].combine(1, 2) shouldBe 3
    Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6)) shouldBe List(1, 2, 3, 4, 5, 6)
    Semigroup[Option[Int]].combine(Option(1), Option(2)) shouldBe Some(3)
    Semigroup[Option[Int]].combine(Option(1), None) shouldBe Some(1)
    Semigroup[Int => Int].combine({(x: Int) => x + 1},{(x: Int) => x * 10}).apply(6) shouldBe 67

    val one = Option(1)
    val two = Option(2)
    val n: Option[Int] = None
    one |+| two shouldBe Some(3)
    one <+> two shouldBe Some(1)
    n |+| two   shouldBe Some(2)
    n <+> two   shouldBe Some(2)
    n |+| n     shouldBe None
    n <+> n     shouldBe None
    two |+| n   shouldBe Some(2)
    two <+> n   shouldBe Some(2)
  }


}
