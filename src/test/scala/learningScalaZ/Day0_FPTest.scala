package learningScalaZ

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by joshr on 14/10/2015.
 */
class Day0_FPTest extends FlatSpec with Matchers {

  "FP" should "Parametric polymorphism " in {
    def head[A](xs: List[A]): A = xs(0)
    head(1 :: 2 :: Nil) should be (1)
    case class Car(make: String)
    head(Car("Civic") :: Car("CR-V") :: Nil) should be (Car("Civic"))
  }

  "FP" should "build up to Monoids 1" in {
    trait Plus[A] {
      def plus(a1: A, a2: A): A
    }

    def plus[A: Plus](a1: A, a2: A): A = implicitly[Plus[A]].plus(a1, a2)

    def sum(xs: List[Int]): Int = xs.foldLeft(0) { _ + _ }

    object IntMonoid {
      def mappend(a: Int, b: Int): Int = a + b  //append
      def mzero: Int = 0                        //zero
    }

    def sum2(xs: List[Int]): Int = xs.foldLeft(IntMonoid.mzero)(IntMonoid.mappend)

    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    object IntMonoid2 extends Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a + b
      def mzero: Int = 0
    }

    def sum3(xs: List[Int], m: Monoid[Int]): Int = xs.foldLeft(m.mzero)(m.mappend)
    def sum4(xs: List[Int])(implicit m: Monoid[Int]): Int = xs.foldLeft(m.mzero)(m.mappend)



    def sum5[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }

    implicit val intMonoid = IntMonoid2
    sum5(List(1, 2, 3, 4)) should be (10)
  }

  "FP" should "Monoid" in {
    trait Monoid[A] {
      def mappend(a1: A, a2: A): A
      def mzero: A
    }

    object Monoid {
      implicit val IntMonoid: Monoid[Int] = new Monoid[Int] {
        def mappend(a: Int, b: Int): Int = a + b
        def mzero: Int = 0
      }
      implicit val StringMonoid: Monoid[String] = new Monoid[String] {
        def mappend(a: String, b: String): String = a + b
        def mzero: String = ""
      }
    }

    def sum[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      xs.foldLeft(m.mzero)(m.mappend)
    }
    sum(List(1, 2, 3, 4)) should be (10)
    sum(List("a", "b", "c")) should be ("abc")

    val multiMonoid: Monoid[Int] = new Monoid[Int] {
      def mappend(a: Int, b: Int): Int = a * b
      def mzero: Int = 1
    }
    sum(List(1, 2, 3, 4))(multiMonoid) should be (24)


    //  apply the same abstraction to pull out FoldLeft type-class:
    object FoldLeftList {
      def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
    }
    def sum2[A: Monoid](xs: List[A]): A = {
      val m = implicitly[Monoid[A]]
      FoldLeftList.foldLeft(xs, m.mzero, m.mappend)
    }
    sum2(List(1, 2, 3, 4)) should be (10)
    sum2(List("a", "b", "c")) should be ("abc")
    sum2(List(1, 2, 3, 4))(multiMonoid) should be (24)

    trait FoldLeft[F[_]] {
      def foldLeft[A, B](xs: F[A], b: B, f: (B, A) => B): B
    }
    object FoldLeft {
      implicit val FoldLeftList: FoldLeft[List] = new FoldLeft[List] {
        def foldLeft[A, B](xs: List[A], b: B, f: (B, A) => B) = xs.foldLeft(b)(f)
      }
    }
    def sum3[M[_]: FoldLeft, A: Monoid](xs: M[A]): A = {
      val m = implicitly[Monoid[A]]
      val fl = implicitly[FoldLeft[M]]
      fl.foldLeft(xs, m.mzero, m.mappend)
    }
    sum3(List(1, 2, 3, 4)) should be (10)
    sum3(List("a", "b", "c")) should be ("abc")

    // method injection:
    def plus[A: Monoid](a: A, b: A): A = implicitly[Monoid[A]].mappend(a, b)
    plus(3, 4)  should be (7)

    trait MonoidOp[A] {
      val F: Monoid[A]
      val value: A
      def |+|(a2: A): A = F.mappend(value, a2)
    }
    implicit def toMonoidOp[A: Monoid](a: A): MonoidOp[A] = new MonoidOp[A] {
      val F = implicitly[Monoid[A]]
      val value = a
    }
    3 |+| 4 should be (7)
    "a" |+| "b" should be ("ab")




  }


}
