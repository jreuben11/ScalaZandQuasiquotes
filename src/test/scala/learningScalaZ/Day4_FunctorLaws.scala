package learningScalaZ
/**
 * Created by joshr on 17/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

class Day4_FunctorLaws  extends FlatSpec with Matchers {
  "ScalaZ Functor Laws" should "uphold identitiy" in {
    val l = List(1, 2, 3)
    l map {identity} should be (l)
  }

  "ScalaZ Foldable" should "Fold" in {
    List(1, 2, 3).foldRight (1) {_ * _} should be (6)
    9.some.foldLeft(2) {_ + _} should be (11)
    val boolTag: @@[Boolean, Tags.Disjunction] = List(true, false, true, true) foldMap {Tags.Disjunction.apply}
    Tag.unwrap(boolTag) should be (true)
  }
}
