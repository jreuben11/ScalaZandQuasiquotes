/**
 * Created by joshr on 22/10/2015.
 */


import org.scalatest.{Matchers, FlatSpec}
import shapeless._
import shapeless.poly._
import shapeless.syntax.zipper._
import shapeless.syntax.std.tuple._
import shapeless.syntax.std.function._
import shapeless.ops.function._



class ShapelessTest extends FlatSpec with Matchers {

  // choose is a function from Sets to Options with no type specific cases
  object choose extends (Set ~> Option) {
    def apply[T](s: Set[T]) = s.headOption
  }
  // size is a function from Ints or Strings or pairs to a 'size' defined
  // by type specific cases
  object ssize extends Poly1 {
    implicit def caseInt = at[Int](x => 1)
    implicit def caseString = at[String](_.length)
    implicit def caseTuple[T, U](implicit st : Case.Aux[T, Int], su : Case.Aux[U, Int]) = at[(T, U)](t => ssize(t._1)+ssize(t._2))
  }

  object addSize extends Poly2 {
    implicit  def default[T](implicit st: ssize.Case.Aux[T, Int]) = at[Int, T]{ (acc, t) => acc+ssize(t) }
  }

  object option extends (Id ~> Option) {
    def apply[T](t: T) = Option(t)
  }

  "shapeless ~> PolyMorphicFunctor" should "transform set head value into option" in {

    choose(Set(1, 2, 3)) should be (Some(1))
    choose(Set('a', 'b', 'c')) should be (Some('a'))

    // apply a PolyMorphicFunctor to 2 sets
    def pairApply(f: Set ~> Option) = (f(Set(1, 2, 3)), f(Set('a', 'b', 'c')))
    pairApply(choose) should be ((Some(1),Some('a')))

    //interoperable with ordinary monomorphic function values
    List(Set(1, 3, 5), Set(2, 4, 6)) map choose should be (List(Some(1), Some(2)))



    ssize(23) should be (1)
    ssize("foo") should be (3)
    ssize((23, "foo")) should be (4)	// size(t._1) goes to caseInt,  size(t._2) goes to caseString
    ssize(((23, "foo"), 13)) should be(5)
  }

  "Shapeless HList" should "Heterogenous list" in {
    val sets = Set(1) :: Set("foo") :: HNil
    val opts = sets map choose  should be (Some(1) :: Some("foo") :: HNil )
    val l = (23 :: "foo" :: HNil) :: HNil :: (true :: HNil) :: HNil
    l flatMap identity should be (23 :: "foo"  :: true :: HNil)

    val l2 = 23 :: "foo" :: (13, "wibble") :: HNil
    l2.foldLeft(0)(addSize) should be (11)

    val l3 = 1 :: "foo" :: 3.0 :: HNil
    l3.toZipper.right.put(("wibble", 45)).reify should be (1 :: ("wibble",45) :: 3.0 :: HNil)
    l3.toZipper.right.delete.reify should be (1 :: 3.0 :: HNil)
    l3.toZipper.last.left.insert("bar").reify should be ( 1 :: "foo" :: "bar" :: 3.0 :: HNil )

  }

  "Shapeless HList" should "support HList-style operations on standard Scala tuples" in {
    (23, "foo", true).head should be (23)
    (23, "foo", true).tail should be (("foo", true))
    //(23, "foo", true).drop(2) should be ( (true) )  //tuple1 blows up !
    (23, "foo", true).take(2) should be ((23,"foo"))
    //(23, "foo", true).split(1)  should be (((23),("foo",true))) //tuple1 blows up !

    // prepend, append, concatenate
    23 +: ("foo", true)should be ((23,"foo",true))
    (23, "foo") :+ true should be  ((23,"foo",true))
    (23, "foo") ++ (true, 2.0)should be ( (23,"foo",true,2.0) )

    (23, "foo", true) map option should be ((Some(23),Some("foo"),Some(true)))
    ((23, "foo"), (), (true, 2.0)) flatMap identity should be ((23,"foo",true,2.0))
    (23, "foo", (13, "wibble")).foldLeft(0)(addSize) should be (11)

    // conversion to `HList`s and ordinary Scala `List`s
    (23, "foo", true).productElements should be (23 :: "foo" :: true :: HNil)
    (23, "foo", true).toList should be (List(23, "foo", true))
    (23, ("foo", true), 2.0).toZipper.right.down.put("bar").root.reify should be ((23,("bar",true),2.0))

  }

  "Shapeless" should "abstract over arity" in {
    def applyProduct[P <: Product, F, L <: HList, R]
    (p: P)(f: F)
    (implicit gen: Generic.Aux[P, L],
     fp: FnToProduct.Aux[F, L => R])
    = f.toProduct(gen.to(p))

    applyProduct(1, 2)((_: Int)+(_: Int)) should be (3)

    applyProduct(1, 2, 3)((_: Int)*(_: Int)*(_: Int)) should be (6)

  }
}
