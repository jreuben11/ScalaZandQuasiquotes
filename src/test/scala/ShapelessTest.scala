/**
 * Created by joshr on 22/10/2015.
 */


import org.scalatest.{Matchers, FlatSpec}
import shapeless._
import shapeless.poly._

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
    import shapeless.syntax.zipper._
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

  "Shapeless HList implicit tuple ops" should "support HList-style operations on standard Scala tuples" in {
    import shapeless.syntax.std.tuple._
    import shapeless.syntax.zipper._
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

  "Shapeless HMap" should "hetrogenous maps enforce Key/value relations" in {
    class BiMapIS[K, V]
    implicit val intToString = new BiMapIS[Int, String]
    implicit val stringToInt = new BiMapIS[String, Int]

    val hm = HMap[BiMapIS](23 -> "foo", "bar" -> 13)
    //val hm2 = HMap[BiMapIS](23 -> "foo", 23 -> 13)   // Does not compile
    hm.get(23) should be (Some("foo"))
    hm.get("bar") should be (Some(13))

    import hm._
    val l = 23 :: "bar" :: HNil
    l map hm should be ("foo" :: 13 :: HNil)
  }

  "Shapeless narrow" should "support Singleton-typed literals" in {
    import shapeless.syntax.std.tuple._
    import shapeless.syntax.singleton._
    import shapeless.syntax.SingletonOps
    val l = 23 :: "foo" :: true :: HNil
    l(1) should be ("foo")
    val t = (23, "foo", true)
    t(1) should be ("foo")

    23.narrow should be (23) // ???
    "foo".narrow should be ("foo")

    val (wTrue, wFalse) = (Witness(true), Witness(false))
    type True = wTrue.T
    type False = wFalse.T
    trait Select[B] { type Out } // typeclass
    implicit val selInt = new Select[True] { type Out = Int }
    implicit val selString = new Select[False] { type Out = String }
    // compilation failed here: Error:(145, 48) type Out is not a member of shapeless.WitnessWith[Select]

//    def select[T](b: WitnessWith[Select])(t: b.Out) = t
//    select(true)(23)
//    select(false)("foo")

    val s1: Symbol = 'foo   // non-singleton type
    val s2 = 'foo.narrow   // singleton type

  }
  "Shapeless record ->>"  should "be extensible" in {
    import shapeless.syntax.singleton._
    import shapeless.record._
    val book =
      ("author" ->> "Benjamin Pierce") ::
      ("title"  ->> "Types and Programming Languages") ::
      ("id"     ->>  262162091) ::
      ("price"  ->>  44.11) ::
      HNil
    book("author") should be ("Benjamin Pierce")
    book("title")   should be("Types and Programming Languages")
    book("id") should be(262162091)
    book("price")   should be(44.11)
    book.keys should be ("author" :: "title" :: "id" :: "price" :: HNil)
    book.values should be ("Benjamin Pierce" :: "Types and Programming Languages" :: 262162091 :: 44.11 :: HNil)
    val newPrice = book("price") + 2.0
    newPrice should be (46.11)
    val updated = book +("price" ->> newPrice) // Update an existing field
    updated should be ("Benjamin Pierce" :: "Types and Programming Languages" :: 262162091 :: 46.11 :: HNil)
    val extended = updated + ("inPrint" ->> true)  // Add a new field
    extended should be ("Benjamin Pierce" :: "Types and Programming Languages" :: 262162091 :: 46.11 :: true :: HNil)
    val noId = extended - "id"  // Removed a field
    noId should be ("Benjamin Pierce" :: "Types and Programming Languages" :: 46.11 :: true :: HNil)



  }

  "Shapeless Coproduct :+:" should "support discriminated unions: mapping, selection and unification" in {

    type ISB = Int :+: String :+: Boolean :+: CNil
    val isb = Coproduct[ISB]("foo")
    isb.select[Int] should be (None)
    isb.select[String] should be (Some("foo"))

    // doesnt compile: RecordType
    //val uSchema = RecordType.like('i ->> 23 :: 's ->> "foo" :: 'b ->> true :: HNil)
    // type U = uSchema.Union
    // val u = Coproduct[U]('s ->> "foo")  // Inject a String into the union at label 's
    //u.get('i) should be (None)

  }

  "Shapeless Generic[T]" should "Generic representation of (sealed families of) case classes" in {
    case class Foo(i: Int, s: String, b: Boolean)
    val foo = Foo(23, "foo", true)
    val fooGen = Generic[Foo]
    fooGen.to(foo) should be( 23 :: "foo" :: true :: HNil)
    val f1 = fooGen.to(foo)
    val f2 = 13 :: f1.tail
    f2 should be (13 :: "foo" :: true :: HNil)
    fooGen.from(f2) should be (Foo(13, "foo", true))

  }






}
