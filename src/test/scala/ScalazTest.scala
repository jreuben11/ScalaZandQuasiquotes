/**
 * Created by joshr on 10/8/15.
 */

import org.scalatest.{Matchers, FlatSpec}

import scalaz._
import Scalaz._



class ScalazTest extends FlatSpec with Matchers {

  "Scalaz" should "Equal[A]" in {
    // def equal(a1: A, a2: A): Boolean
    //(1 === 2)
    (2 =/= 1) assert_=== true
    (2 =/= 1)     should be (true)
  }

  "Scalaz" should "Order[A]" in {
    // def order(x: A, y: A): Ordering
    1.0 ?|? 2.0   should be (Ordering.LT)
    1.0 lt 2.0    should be (true)
    1.0 gt 2.0    should be (false)
    1.0 lte 2.0   should be (true)
    1.0 gte 2.0   should be (false)
    1.0 max 2.0   should be (2.0)
    1.0 min 2.0   should be (1.0)
  }

  "Scalaz" should "Show[A]" in {
    // def show(f: A): Cord
    //1.0.show      should be (Cord("1.0"))     //1.0 was not equal to 1.0 ???
    1.0.shows     should be ("1.0")
    1.0.print     should be (())
    1.0.println   should be (())
  }

  "Scalaz" should "Enum[A] extends Order[A]" in {
    //def pred(a: A): A
    //def succ(a: A): A
    1 |-> 2 should be (List(1, 2))
    1 |--> (2, 5) should be (List(1, 3, 5))

    // |=>/|==>/from/fromStep return EphemeralStream[A]
    (1 |=> 2).toList should be (List(1, 2))
    (1 |==> (2, 5)).toList should be(List(1, 3, 5))
    (1.from take 2).toList should be (List(1, 2))
    ((1 fromStep 2) take 2).toList should be (List(1, 3))
    1.pred should be (0)
    1.predx should be (Some(0))
    1.succ should be (2)
    1.succx should be (Some(2))
    1 -+- 1 should be (2)
    1 --- 1 should be (0)
    Enum[Int].min assert_=== Some(-2147483648)
    Enum[Int].max assert_=== Some(2147483647)
  }

  "Scalaz" should "Semigroup[A]" in {
    //def append(a1: A, a2: => A): A
    List(1, 2) |+| List(3) should be (List(1, 2, 3))
    List(1, 2) mappend List(3) should be(List(1, 2, 3))
    1 |+| 2 should be (3)
    (Tags.Multiplication(2) |+| Tags.Multiplication(3)) should be (6)

    // Tags.Disjunction (||), Tags.Conjunction (&&)

     val x: @@[Boolean, Tags.Disjunction] = Tags.Disjunction(true) |+| Tags.Disjunction(false)
    //val y:Boolean =
    //assert_=== true

    //  (Tags.Conjunction(true) |+| Tags.Conjunction(false)) assert_=== false
    //(Ordering.LT: Ordering) |+| (Ordering.GT: Ordering) assert_=== Ordering.LT
    (none: Option[String]) |+| "andy".some assert_=== "andy".some
    // (Tags.First('a'.some) |+| Tags.First('b'.some)) assert_=== 'a'.some
    //(Tags.Last('a'.some) |+| Tags.Last(none: Option[Char])) assert_=== 'a'.some
  }

  "Scalaz" should "Monoid[A] extends Semigroup[A]" in {
    //def zero: A
    mzero[List[Int]] assert_=== Nil
  }
}
