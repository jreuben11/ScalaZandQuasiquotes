package learningCats

import org.scalatest.{Matchers, FlatSpec}

import cats.data.{Validated, Xor}
import cats.syntax.apply
import cats.syntax.apply._  // For |@| syntax

import cats._, cats.std.all._, cats.syntax.all._

import cats.data.Xor.{left,right}
import cats.data.Validated.{Invalid, invalid, valid}
/**
  * Created by joshr on 01/04/2016.
  */
class catsDataTest extends FlatSpec with Matchers{
  "Xor" should "right-biased Either" in {

    type Result[A] = String Xor A
    val l: Result[Int] = left("Failed")
    val r: Result[Int] = right(1)

    // Nothing happens when we map a left
    l.map(x => x + 1) shouldBe (left("Failed"))
    l.map(x => x + 1).toOption shouldBe (None)
    // The right is transformed
    r.map(x => x + 1).toOption shouldBe (Some(2))
    r.map(x => x + 1) shouldBe (right(2))
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
    x shouldBe (left(List("Stops here")))

    // Accumulates all the errors
     val v = (v1 |@| v2 |@| v3) map { _ + _ + _ }

    v shouldBe (Invalid(List("Accumulates this", "And this")))
    println(v)


  }
}
