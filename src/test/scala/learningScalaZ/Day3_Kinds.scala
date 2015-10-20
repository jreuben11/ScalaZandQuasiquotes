package learningScalaZ

/**
 * Created by joshr on 17/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz._
import Scalaz._

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
