/**
  * Created by joshr on 13/02/2016.
  */
import org.scalatest.{Matchers, FlatSpec}

class BasicsTest extends FlatSpec with Matchers {

  "Existential Types" should "support references to type variables that are unknown" in {
    class X[a](val a1: a)
    trait Y
    trait Z extends Y

    def F(x: X[_ <: Y]) = x // no need for defining X[+a]
    def F_verbose(x: X[y] forSome {type y <: Y}) = x

    val xOfz = new X(new Z{})

    F(xOfz) should be (xOfz)

  }

  "for comprehensions" should "be shorthand for map ops" in {
    val xs = 1 to 5
    val ys = 1 until 6 // same
    val zs = 1 to 10 by 2
    xs should be (ys)

    // filter/map
    (for (x <- xs if x%2 == 0) yield x*10)    should be (xs.filter(_%2 == 0).map(_*10))
    // destructuring bind
    (for ((x,y) <- xs zip ys)  yield x*y)     should be ((xs zip ys) map { case (x,y) => x*y })
    // cross product
    (for (x <- xs; y <- ys)    yield x*y)     should be (xs flatMap {x => ys map {y => x*y}})
  }

}
