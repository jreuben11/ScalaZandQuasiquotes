import commonAlgos._
import org.scalatest.FunSuite

/**
  * Created by joshr on 01/03/2016.
  */
class CommonAlgoTest extends FunSuite{

  test ("iterativeBinarySearch"){
    val n = 100
    val odds = 1 to n by 2
    val result = (0 to n).flatMap(BinarySearch.iterative(odds, _))
    assert(result === (0 until odds.size))
  }

  test ("recursiveBinarySearch"){
    val n = 100
    val odds = 1 to n by 2
    val result = (0 to n).flatMap(BinarySearch.recursive(odds, _))
    assert(result === (0 until odds.size))
  }

  test("recursiveSubstringCount"){
    assert(SubstringCount.recursive("ababababab", "abab") === 2)
    assert(SubstringCount.recursive("the three truths", "th") === 3)
  }

  test("regexSubstringCount"){
    assert(SubstringCount.regex("ababababab", "abab") === 2)
    assert(SubstringCount.regex("the three truths", "th") === 3)
  }

  test("Dijkstra") {
    import commonAlgos.Dijkstra._
    val lookup = Map(
      "a" -> List((7.0, "b"), (9.0, "c"), (14.0, "f")),
      "b" -> List((10.0, "c"), (15.0, "d")),
      "c" -> List((11.0, "d"), (2.0, "f")),
      "d" -> List((6.0, "e")),
      "e" -> List((9.0, "f")),
      "f" -> Nil
    )
    val res = Dijkstra[String](lookup, List((0D, List("a"))), "e", Set())
    assert(res === (26.0,List(
      "a",
      "c",
      "d",
      "e"
    )))
  }

  test("fibonacci"){
    import Fibonacci._
    val fib12 = Vector(0,1,1,2,3,5,8,13,21,34,55,89,144)
    assert((0 to 12).map(recursiveFib(_)) === fib12)
    assert((0 to 12).map(lazyFib(_)) === fib12)
    assert((0 to 12).map(tailrecursiveFib(_)) === fib12)
    assert((0 to 12).map(foldFib(_)) === fib12)
    assert(iterativeFib.take(13).toVector === fib12)
  }

  test("permutations"){
    import Permutations._
    val builtInPermutations = List('a, 'b, 'c).permutations.toList
    val expectedPermutations = List(
      List('a, 'b, 'c),
        List('a, 'c, 'b),
        List('b, 'a, 'c),
        List('b, 'c, 'a),
        List('c, 'a, 'b),
        List('c, 'b, 'a)
    )
    assert (builtInPermutations === expectedPermutations)
    assert(permutations(List('a, 'b, 'c)) === expectedPermutations)
  }
}
