package learningScalaZ

/**
 * Created by joshr on 18/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
//import scala.language.higherKinds
//import scala.language.implicitConversions
//import scala.language.postfixOps

import scalaz._
import Scalaz._

class Day7_ApplicativeBuilder extends FlatSpec with Matchers {
  "ScalaZ AB" should "work" in {
    (3.some |@| 5.some) {_ + _} should be(8.some)
    val f = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
    f(1) should be(12)
  }

  "ScalaZ State" should "just a Stateful Stack" in {
    type Stack = List[Int]
    def pop(stack: Stack): (Int, Stack) = stack match {
      case x :: xs => (x, xs)
    }
    def push(a: Int, stack: Stack): (Unit, Stack) = ((), a :: stack)
    def stackManip(stack: Stack): (Int, Stack) = {
      val (_, newStack1) = push(3, stack)
      val (a, newStack2) = pop(newStack1)
      pop(newStack2)
    }
    stackManip(List(5, 8, 2, 1)) should be((5, List(8, 2, 1)))
  }

  "ScalaZ State" should "Stack using State" in {
    type Stack = List[Int]
    val pop = State[Stack, Int] { case x :: xs => (xs, x) }
    def push(a: Int) = State[Stack, Unit] { case xs => (a :: xs, ()) }
    def stackManip: State[Stack, Int] = for {
      _ <- push(3)
      a <- pop
      b <- pop
    } yield (b)
    stackManip(List(5, 8, 2, 1)) should be((List(8, 2, 1), 5))

    def stackyStack: State[Stack, Unit] = for {
      stackNow <- get
      r <- if (stackNow == List(1, 2, 3)) put(List(8, 3, 1)) else put(List(9, 2, 1))
    } yield r
    stackyStack(List(1, 2, 3)) should be((List(8, 3, 1), ()))

    val pop2: State[Stack, Int] = for {
      s <- get[Stack]
      (x :: xs) = s
      _ <- put(xs)
    } yield x
    def push2(x: Int): State[Stack, Unit] = for {
      xs <- get[Stack]
      r <- put(x :: xs)
    } yield r

  }

  "ScalaZ Either" should "work" in {
    // vanilla is not a monad:
    Left[String, Int]("boom").right flatMap { x => Right[String, Int](x + 1) } should be(Left("boom"))

    val either1: \/[String, Int] = 1.right[String]
    either1 should be(\/-(1))
    val either2: \/[Int, Int] = 2.left[Int] // doesnt work for string  / Int :(
    either2 should be(-\/(2))


    //    "boom".left[Int] >>= { x => (x + 1).right }
  }

  "ScalaZ Validation" should "work" in {
    "event 1 ok".success[String] should be(Success("event 1 ok"))
    "event 1 failed".failure[String] should be(Failure("event 1 failed"))


    1.wrapNel should be(NonEmptyList(1))

    val s1: ValidationNel[String, String] = "event 1 ok".successNel[String]
    val f1: ValidationNel[String, String] = "event 2 failed!".failureNel[String]
    val f2: ValidationNel[String, String] = "event 3 failed!".failureNel[String]
    val x = s1 |@| f1 |@| f2
    //TODO: x should be (Failure("event 2 failed!event 3 failed!"))

  }
}




