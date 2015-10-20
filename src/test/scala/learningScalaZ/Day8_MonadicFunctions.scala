package learningScalaZ

/**
 * Created by joshr on 20/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
//import scala.language.higherKinds
//import scala.language.implicitConversions
//import scala.language.postfixOps

import scalaz._
import Scalaz._

class Day8_MonadicFunctions extends FlatSpec with Matchers {

  "ScalaZ join" should "flatten nested monadic values" in {
    (Some(9.some): Option[Option[Int]]).join should be (9.some)
    List(List(1, 2, 3), List(4, 5, 6)).join should be (List(1, 2, 3, 4, 5, 6))
    9.right[String].right[String].join should be (\/-(9))
    //val y = "boom".left[Int].right[String].join // string and either problem
  }

  "ScalaZ filterM" should "filter any monad" in {
    List(1, 2, 3) filterM { x => List(true, false) } should be  (List(
      List(1, 2, 3),
      List(1, 2),
      List(1, 3),
      List(1),
      List(2, 3),
      List(2),
      List(3),
      List()
    ))
  }

  "ScalaZ foldLeftM" should "fold any monad" in {
    def binSmalls(acc: Int, x: Int): Option[Int] = {
      if (x > 9) (none: Option[Int])
      else (acc + x).some
    }
    List(2, 8, 3, 1).foldLeftM(0) {binSmalls} should be (Some(14))
    List(2, 8, 10, 1).foldLeftM(0) {binSmalls} should be (None)
  }

  "ScalaZ Kliesli" should "compose functions" in {
    val f: Kleisli[Option, Int, Int] = Kleisli { (x: Int) => (x + 1).some }
    val g: Kleisli[Option, Int, Int] = Kleisli { (x: Int) => (x * 100).some }
    (4.some >>= (f <=< g)) should be(Some(401)) // f compose g
    (4.some >>= (f >=> g)) should be(Some(500)) // f andThen g
  }

}


