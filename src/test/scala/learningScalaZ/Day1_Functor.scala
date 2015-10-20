package learningScalaZ

/**
 * Created by joshr on 14/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._
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
