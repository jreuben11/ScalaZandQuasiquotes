package learningScalaZ

/**
 * Created by joshr on 18/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.postfixOps
import scalaz._
import Scalaz._

class Day6_WriterReader extends FlatSpec with Matchers {
  "ScalaZ Reader" should "vanilla Scala: implicit class implicitly does ctor conversion, implicitly pulls the function into f" in {
    def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")
    implicit class PairOps[A](pair: (A, String)) {
      def applyLog[B](f: A => (B, String)): (B, String) = {
        val (x, log) = pair //from ctor param
        val (y, newlog) = f(x)
        (y, log ++ newlog)
      }
    }
    //
    (3, "Smallish gang.") applyLog isBigGang  should be ((false,"Smallish gang.Compared gang size to 9."))
    // same:
    (PairOps[Int](3, "Smallish gang.")).applyLog(isBigGang) should be ((false,"Smallish gang.Compared gang size to 9."))
  }

  "ScalaZ Reader" should "read over any Monoid, not just tuple (Boolean, String)" in {
    def isBigGang(x: Int): (Boolean, String) = (x > 9, "Compared gang size to 9.")
    implicit class PairOps[A, B: Monoid](pair: (A, B)) {
      def applyLog[C](f: A => (C, B)): (C, B) = {
        val (x, log) = pair
        val (y, newlog) = f(x)
        (y, log |+| newlog)
      }
    }
    (3, "Smallish gang.") applyLog isBigGang  should be ((false,"Smallish gang.Compared gang size to 9."))
  }

  "ScalaZ Writer" should "set and tell, write intermediate results of for" in {
    val w1: Writer[String, Int] = 3.set("Smallish gang.")
    val w2: Writer[String, Unit] = "something".tell
    // Broke: MonadTell[Writer, String].point(3).run should be ("",3)

    def logNumber(x: Int): Writer[List[String], Int] = x.set(List("Got number: " + x.shows))
    def multWithLog: Writer[List[String], Int] = for {
      a <- logNumber(3)
      b <- logNumber(5)
    } yield a * b
    (multWithLog run) should be ((
      List(
        "Got number: 3",
        "Got number: 5"),
        15
      )
    )
  }
  "ScalaZ Writer" should "capture intermediate values of recursion" in {
    def gcd(a: Int, b: Int): Writer[List[String], Int] = {
      if (b == 0)
        for {
          _ <- List("Finished with " + a.shows).tell
        } yield a
      else
        (List(
          a.shows + " mod " + b.shows + " = " +
          (a % b).shows).tell) >>= { _ => gcd(b, a % b) } //recursion
    }
    (gcd(8, 3).run) should be (
      (List(
        "8 mod 3 = 2",
        "3 mod 2 = 1",
        "2 mod 1 = 0",
        "Finished with 1"
      ),1 )
    )
  }

  "ScalaZ for" should "a function is not just a Functor, it is also a Reader Monad" in {
    val f = (_: Int) * 5
    val g = (_: Int) + 3
    (g map f)(8) should be (55)
    val f2 = ({(_: Int) * 2} |@| {(_: Int) + 10}) {_ + _}
    f2(3) should be (19) // 3*2 + 3+10
    val addStuff = for {
        a <- (_: Int) * 2
        b <- (_: Int) + 10
      } yield a + b
    addStuff(3) should be (19)
  }
}
