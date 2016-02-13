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

}
