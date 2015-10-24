/**
 * Created by joshr on 22/10/2015.
 */


import org.scalatest.{Matchers, FlatSpec}
import shapeless._
import shapeless.poly._
// import shapeless.test.illTyped





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
    import shapeless.syntax.std.function._
    import shapeless.ops.function._
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
    fooGen.to(foo) should be(23 :: "foo" :: true :: HNil)
    val f1 = fooGen.to(foo)
    val f2 = 13 :: f1.tail
    f2 should be(13 :: "foo" :: true :: HNil)
    fooGen.from(f2) should be(Foo(13, "foo", true))

  }

  "Shapeless lift function to Poly ->" should "Transform tree by applying poly everywhere" in {
    // Simple recursive case class family
    sealed trait Tree[T]
    case class Leaf[T](t: T) extends Tree[T]
    case class Node[T](left: Tree[T], right: Tree[T]) extends Tree[T]
    val tree: Tree[Int] =
      Node(
        Node(
          Node(
            Leaf(1),
            Node(
              Leaf(2),
              Leaf(3)
            )
          ),
          Leaf(4)
        ),
        Node(
          Leaf(5),
          Leaf(6)
        )
      )

    val tree2: Tree[Int] =
      Node(
        Node(
          Node(
            Leaf(2),
            Node(
              Leaf(3),
              Leaf(4)
            )
          ),
          Leaf(5)
        ),
        Node(
          Leaf(6),
          Leaf(7)
        )
      )

    // Polymorphic function which adds 1 to any Int and is the identity on all other values
    object inc extends ->((i: Int) => i+1)
    // Transform tree by applying inc everywhere
    // FAILS TO COMPILE:
    //val x = everywhere(inc)(tree) should be (tree2)
  }

  "Shapeless LabeledGeneric[T]" should "support symbolic access, type safe operations on fields" in {
    import shapeless.record._
    import shapeless.syntax.singleton._
    case class Book(author: String, title: String, id: Int, price: Double)
    val tapl = Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11)

    val bookGen = LabelledGeneric[Book]
    val rec = bookGen.to(tapl) // Convert case class value to generic representation
    rec should be ("Benjamin Pierce" :: "Types and Programming Languages" :: 262162091 :: 44.11 :: HNil)
    rec('price) should be (44.11) // Access the price field symbolically, maintaining type information
    // type safe operations on fields
    bookGen.from(rec.updateWith('price)(_+2.0)) should be (Book("Benjamin Pierce", "Types and Programming Languages", 262162091, 46.11))

    case class ExtendedBook(author: String, title: String, id: Int, price: Double, inPrint: Boolean)
    val bookExtGen = LabelledGeneric[ExtendedBook]
    val rec2 = bookExtGen.from(rec + ('inPrint ->> true))  // map values between case classes via generic representation
    rec2 should be (ExtendedBook("Benjamin Pierce", "Types and Programming Languages", 262162091, 44.11, true))
  }

  "shapeless lens[T] >>" should "work" in {
    // A pair of ordinary case classes ...
    case class Address(street : String, city : String, postcode : String)
    case class Person(name : String, age : Int, address : Address)

    // Some lenses over Person/Address ...
    val nameLens     = lens[Person] >> 'name
    val ageLens      = lens[Person] >> 'age
    val addressLens  = lens[Person] >> 'address
    val streetLens   = lens[Person] >> 'address >> 'street
    val cityLens     = lens[Person] >> 'address >> 'city
    val postcodeLens = lens[Person] >> 'address >> 'postcode

    val person = Person("Joe Grey", 37, Address("Southover Street", "Brighton", "BN2 9UA"))
    ageLens.get(person) should be (37)
    val person2 = ageLens.set(person)(38)
    person2.age should be (38)
    val person3 = ageLens.modify(person2)(_ + 1)
    person3.age should be (39)

  }

  // COMPILATION FAIL: |+| is not a member of Foo
//  "shapeless monoid |+|" should "Automatic type class instance derivation" in {
//    import scalaz._
//    import Scalaz._
//
//    // A pair of arbitrary case classes
//    case class Foo(i : Int, s : String)
//    case class Bar(b : Boolean, s : String, d : Double)
//
//    Foo(13, "foo") |+| Foo(23, "bar") should be ( Foo(36,"foobar") )
//
//    Bar(true, "foo", 1.0) |+| Bar(false, "bar", 3.0) should be( Bar(true,"foobar",4.0) )
//  }

  "Shapeless" should "First class lazy values tie implicit recursive knots" in {
    // Simple cons list
    sealed trait List[+T]
    case class Cons[T](hd: T, tl: List[T]) extends List[T]
    sealed trait Nil extends List[Nothing]
    case object Nil extends Nil

    // TypeClass
    trait Show[T] {
      def apply(t: T): String
    }
    object Show {
      // Base case for Int
      implicit def showInt: Show[Int] = new Show[Int] {
        def apply(t: Int) = t.toString
      }

      // Base case for Nil
      implicit def showNil: Show[Nil] = new Show[Nil] {
        def apply(t: Nil) = "Nil"
      }

      // Case for Cons[T]: note (mutually) recursive implicit argument referencing Show[List[T]]
      implicit def showCons[T](implicit st: Lazy[Show[T]], sl: Lazy[Show[List[T]]]): Show[Cons[T]] = new Show[Cons[T]] {
        def apply(t: Cons[T]) = s"Cons(${show(t.hd)(st.value)}, ${show(t.tl)(sl.value)})"
      }

      // Case for List[T]: note (mutually) recursive implicit argument referencing Show[Cons[T]]
      implicit def showList[T](implicit sc: Lazy[Show[Cons[T]]]): Show[List[T]] = new Show[List[T]] {
        def apply(t: List[T]) = t match {
          case n: Nil => show(n)
          case c: Cons[T] => show(c)(sc.value)
        }
      }
    }
    def show[T](t: T)(implicit s: Show[T]) = s(t)

    val l: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))

    // Without the Lazy wrappers above the following would diverge ...
    show(l)  should be  ("Cons(1, Cons(2, Cons(3, Nil)))")

  }

  "shapeless Sized" should "supports Collections with statically known sizes" in {
    def row(cols : Seq[String]) = cols.mkString(",")

    def csv[N <: Nat]
    (hdrs : Sized[Seq[String], N],
     rows : List[Sized[Seq[String], N]]) = row(hdrs) :: rows.map(row(_))

    val hdrs = Sized("Title", "Author")

    val rows = List(
      Sized("Types and Programming Languages", "Benjamin Pierce"),
      Sized("The Implementation of Functional Programming Languages", "Simon Peyton-Jones")
    )

    // hdrs and rows statically known to have the same number of columns
    val formatted = csv(hdrs, rows)                        // Compiles
  }

  "shapeless typeable cast" should "support Type safe cast" in {
    import shapeless.syntax.typeable._
    val l: Any = List(Vector("foo", "bar", "baz"), Vector("wibble"))
    l.cast[List[Vector[String]]] should be (Some(l))
    l.cast[List[Vector[Int]]] should be (None)
    l.cast[List[List[String]]] should be (None)

    //Typeable extractors allow more precision in pattern matches
    val `List[String]` = TypeCase[List[String]]
    val `List[Int]` = TypeCase[List[Int]]

    val li =  List(1, 2, 3)
    def typematch(li: Any) = li match {
      case `List[String]`(List(s, _*)) => s.length
      case `List[Int]`(List(i, _*))    => i+1
    }
    typematch(li) should be (2)
  }

}
