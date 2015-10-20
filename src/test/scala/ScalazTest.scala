/**
 * Created by joshr on 10/8/15.
 */

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.immutable.Stream.cons
import scalaz._
import Scalaz._



class ScalazTest extends FlatSpec with Matchers {

  "Scalaz" should "Equal[A]" in {
    // def equal(a1: A, a2: A): Boolean
    //(1 === 2)
    (2 =/= 1) assert_=== true
    (2 =/= 1)     should be (true)
  }

  "Scalaz" should "Order[A]" in {
    // def order(x: A, y: A): Ordering
    1.0 ?|? 2.0   should be (Ordering.LT)
    1.0 lt 2.0    should be (true)
    1.0 gt 2.0    should be (false)
    1.0 lte 2.0   should be (true)
    1.0 gte 2.0   should be (false)
    1.0 max 2.0   should be (2.0)
    1.0 min 2.0   should be (1.0)
  }

  "Scalaz" should "Show[A]" in {
    // def show(f: A): Cord
    //1.0.show      should be (Cord("1.0"))     //1.0 was not equal to 1.0 ???
    1.0.shows     should be ("1.0")
    1.0.print     should be (())
    1.0.println   should be (())
  }

  "Scalaz" should "Enum[A] extends Order[A]" in {
    //def pred(a: A): A
    //def succ(a: A): A
    1 |-> 2 should be (List(1, 2))
    1 |--> (2, 5) should be (List(1, 3, 5))

    // |=>/|==>/from/fromStep return EphemeralStream[A]
    (1 |=> 2).toList should be (List(1, 2))
    (1 |==> (2, 5)).toList should be(List(1, 3, 5))
    (1.from take 2).toList should be (List(1, 2))
    ((1 fromStep 2) take 2).toList should be (List(1, 3))
    1.pred should be (0)
    1.predx should be (Some(0))
    1.succ should be (2)
    1.succx should be (Some(2))
    1 -+- 1 should be (2)
    1 --- 1 should be (0)
    Enum[Int].min assert_=== Some(-2147483648)
    Enum[Int].max assert_=== Some(2147483647)
  }

  "Scalaz" should "Semigroup[A]" in {
    //def append(a1: A, a2: => A): A
    List(1, 2) |+| List(3) should be (List(1, 2, 3))
    List(1, 2) mappend List(3) should be(List(1, 2, 3))
    1 |+| 2 should be (3)
    (Tags.Multiplication(2) |+| Tags.Multiplication(3)) should be (6)

    // Tags.Disjunction (||), Tags.Conjunction (&&)
    Tag.unwrap((Tags.Disjunction(true) |+| Tags.Disjunction(false))) should be (true)
    Tag.unwrap((Tags.Conjunction(true) |+| Tags.Conjunction(false))) should be ( false)
    (Ordering.LT: Ordering) |+| (Ordering.GT: Ordering) should be (Ordering.LT)
    (none: Option[String]) |+| "andy".some should be ("andy".some)
    (Tags.First('a'.some) |+| Tags.First('b'.some)) should be ('a'.some)
    (Tags.Last('a'.some) |+| Tags.Last(none: Option[Char])) should be ( 'a'.some)
  }

  "Scalaz" should "Monoid[A] extends Semigroup[A]" in {
    //def zero: A
    mzero[List[Int]] should be (Nil)
  }

  "Scalaz" should "Functor[F[_]]" in {
    // def map[A, B](fa: F[A])(f: A => B): F[B]
    List(1, 2, 3) map {_ + 1} should be (List(2, 3, 4))
    List(1, 2, 3) ∘ {_ + 1} should be ( List(2, 3, 4))
    List(1, 2, 3) >| "x" should be ( List("x", "x", "x"))
    List(1, 2, 3) as "x" should be (List("x", "x", "x"))
    List(1, 2, 3).fpair should be (List((1,1), (2,2), (3,3)))
    List(1, 2, 3).strengthL("x") should be (List(("x",1), ("x",2), ("x",3)))
    List(1, 2, 3).strengthR("x") should be (List((1,"x"), (2,"x"), (3,"x")))
    List(1, 2, 3).void should be (List((), (), ()))
    Functor[List].lift {(_: Int) * 2} (List(1, 2, 3)) should be (List(2, 4, 6))
  }

  "Scalaz" should "Pointed[F[_]] extends Functor[F]" in {
    // def point[A](a: => A): F[A]
    1.point[List] should be (List(1))
    1.η[List] should be (List(1))
  }

  "Scalaz" should "Product/Composition" in {
    (Applicative[Option] product Applicative[List]).point(0) should be ((0.some, List(0)))
    (Applicative[Option] compose Applicative[List]).point(0) should be (List(0).some)
  }

  "Scalaz" should "Bind[F[_]] extends Apply[F]" in {
    // def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    3.some flatMap { x => (x + 1).some } should be (4.some)
    (3.some >>= { x => (x + 1).some }) should be (4.some)
    3.some >> 4.some should be (4.some)
    List(List(1, 2), List(3, 4)).join should be (List(1, 2, 3, 4))
  }

  "Scalaz" should "Monad[F[_]] extends Applicative[F] with Bind[F]" in {
    // no contract function
    // failed pattern matching produces None
    (for {(x :: xs) <- "".toList.some} yield x) should be (none)
    (for { n <- List(1, 2); ch <- List('a', 'b') } yield (n, ch)) should be (List((1, 'a'), (1, 'b'), (2, 'a'), (2, 'b')) )
    (for { a <- (_: Int) * 2; b <- (_: Int) + 10 } yield a + b)(3) should be (19)
    List(1, 2) filterM { x => List(true, false) } should be (List(List(1, 2), List(1), List(2), List()))
  }

  "Scalaz" should "Plus[F[_]]" in {
    // def plus[A](a: F[A], b: => F[A]): F[A]
    List(1, 2) <+> List(3, 4) should be (List(1, 2, 3, 4))
  }

  "Scalaz" should "PlusEmpty[F[_]] extends Plus[F]" in {
    //def empty[A]: F[A]
    (PlusEmpty[List].empty: List[Int]) should be (Nil)
  }
  "Scalaz" should "MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F]" in {
    // no contract function
    List(1, 2, 3) filter {_ > 2} should be (List(3))
  }

  "Scalaz" should "Foldable[F[_]]" in {
    //def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B
    //def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
    List(1, 2, 3).foldRight(0) {_ + _} assert_=== 6
    List(1, 2, 3).foldLeft(0) {_ + _} assert_=== 6

//    (List(1, 2, 3) foldMap {
//      Tags.Multiplication
//    }: Int) assert_=== 6

    List(1, 2, 3).foldLeftM(0) { (acc, x) => (acc + x).some } assert_=== 6.some
  }

  "Scalaz" should "Traverse[F[_]] extends Functor[F] with Foldable[F]" in {
    //def traverseImpl[G[_] : Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
    List(1, 2, 3) traverse { x => (x > 0) option (x + 1) } assert_=== List(2, 3, 4).some
    List(1, 2, 3) traverseU {_ + 1} assert_=== 9
    List(1.some, 2.some).sequence assert_=== List(1, 2).some
    1.success[String].leaf.sequenceU map {_.drawTree} assert_=== "1\n".success[String]
  }

  "Scalaz" should "Length[F[_]]" in {
    //def length[A](fa: F[A]): Int
    List(1, 2, 3).length assert_=== 3
  }

  "Scalaz" should "Index[F[_]]" in {
    //def index[A](fa: F[A], i: Int): Option[A]
//    List(1, 2, 3) index 2 assert_=== 3.some
//    List(1, 2, 3) index 3 assert_=== none
  }

  "Scalaz" should " ArrId[=>:[_, _]]" in {
    // def id[A]: A =>: A
  }

  "Scalaz" should " Compose[=>:[_, _]]" in {
    // def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
    val f1 = (_: Int) + 1
    val f2 = (_: Int) * 100
    (f1 >>> f2)(2) assert_=== 300
    (f1 <<< f2)(2) assert_=== 201
  }

  "Scalaz" should "Category[=>:[_, _]] extends ArrId[=>:] with Compose[=>:]" in {
    // no contract function
  }

  "Scalaz" should "Arrow[=>:[_, _]] extends Category[=>:]" in {
    //def arr[A, B](f: A => B): A =>: B
    //def first[A, B, C](f: (A =>: B)): ((A, C) =>: (B, C))
    val f1 = (_: Int) + 1
    val f2 = (_: Int) * 100
    (f1 *** f2)(1, 2) assert_===(2, 200)
    (f1 &&& f2)(1) assert_===(2, 100)
  }

  "Scalaz" should "Unapply[TC[_[_]], MA]" in {
//    type M[_]
//    type A
//    def TC: TC[M]
//    def apply(ma: MA): M[A]
    implicitly[Unapply[Applicative, Int => Int]].TC.point(0).asInstanceOf[Int => Int](10) assert_=== Applicative[({type l[x] = Function1[Int, x]})#l].point(0)(10)
    List(1, 2, 3) traverseU { (x: Int) => {(_: Int) + x} } apply 1 assert_=== List(2, 3, 4) // traverse won't work
  }

  "Scalaz" should "Boolean" in {
    false /\ true assert_=== false // &&
    false \/ true assert_=== true // ||
    (1 < 10) option 1 assert_=== 1.some
    (1 > 10) ? 1 | 2 assert_=== 2
    (1 > 10) ?? {
      List(1)
    } assert_=== Nil
  }

  "Scalaz" should "Option" in {
    1.some assert_=== Some(1)
    none[Int] assert_=== (None: Option[Int])
    1.some ? 'x' | 'y' assert_=== 'x'
    1.some | 2 assert_=== 1 // getOrElse
  }

  "Scalaz" should "Id[+A] = A" in {
    // no contract function
    1 + 2 + 3 |> {
      _ * 6
    }
    1 visit { case x@(2 | 3) => List(x * 2) }
  }

  "Scalaz" should "Tagged[A]" in {
    sealed trait KiloGram
    def KiloGram[A](a: A): A @@ KiloGram = Tag[A, KiloGram](a)
    //def f[A](mass: A @@ KiloGram): A @@ KiloGram
  }

  "Scalaz" should "Tree[A]/TreeLoc[A]" in {
    val tree = 'A'.node('B'.leaf, 'C'.node('D'.leaf), 'E'.leaf)
    (tree.loc.getChild(2) >>= {
      _.getChild(1)
    } >>= {
      _.getLabel.some
    }) assert_=== 'D'.some
    (tree.loc.getChild(2) map {
      _.modifyLabel({ _ => 'Z' })
    }).get.toTree.drawTree assert_=== 'A'.node('B'.leaf, 'Z'.node('D'.leaf), 'E'.leaf).drawTree
  }

  "Scalaz" should "Stream[A]/Zipper[A]" in {
  (Stream(1, 2, 3, 4).toZipper >>= {_.next} >>= {_.focus.some}) assert_=== 2.some
  (Stream(1, 2, 3, 4).zipperEnd >>= {_.previous} >>= {_.focus.some}) assert_=== 3.some
  (for { z <- Stream(1, 2, 3, 4).toZipper; n1 <- z.next } yield { n1.modify {_ => 7} }) map { _.toStream.toList } getOrElse Nil assert_=== List(1, 7, 3, 4)
  unfold(3) { x => (x =/= 0) option (x, x - 1) }.toList assert_=== List(3, 2, 1)
  }

  "Scalaz" should "DList[A]" in {
    DList.unfoldr(3, { (x: Int) => (x =/= 0) option(x, x - 1) }).toList assert_=== List(3, 2, 1)
  }

  "Scalaz" should "Lens[A, B] = LensT[Id, A, B]" in {
    case class Point(x: Double, y: Double)
    case class Turtle(position: Point, d: Double)
    val t0 = Turtle(Point(0.0, 0.0), 0.0)
    val t1 = Turtle(Point(1.0, 0.0), 0.0)
    val turtlePosition = Lens.lensu[Turtle, Point] (
  (a, value) => a.copy(position = value),
  _.position)
    val pointX = Lens.lensu[Point, Double] (
  (a, value) => a.copy(x = value),
  _.x)
    val turtleX = turtlePosition >=> pointX
    turtleX.get(t0) assert_=== 0.0
//    turtleX.set(t0, 5.0) assert_=== Turtle(Point(5.0, 0.0), 0.0)
//    turtleX.mod(_ + 1.0, t0) assert_=== t1
//    t0 |> (turtleX =>= {
//      _ + 1.0
//    }) assert_=== t1
//    (for {x <- turtleX %= {
//      _ + 1.0
//    }} yield x) exec t0 assert_=== t1
//    (for {x <- turtleX := 5.0} yield x) exec t0 assert_=== Turtle(Point(5.0, 0.0), 0.0)
//    (for {x <- turtleX += 1.0} yield x) exec t0 assert_=== t1
  }

  "Scalaz" should "Validation[+E, +A]" in {
    (1.success[String] |@| "boom".failure[Int] |@| "boom".failure[Int]) {_ |+| _ |+| _} assert_=== "boomboom".failure[Int]
    (1.successNel[String] |@| "boom".failureNel[Int] |@| "boom".failureNel[Int]) {_ |+| _ |+| _} assert_=== NonEmptyList("boom", "boom").failure[Int]
    "1".parseInt.toOption assert_=== 1.some
  }

  "Scalaz" should "Writer[+W, +A] = WriterT[Id, W, A]" in {
    (for {x <- 1.set("log1"); _ <- "log2".tell} yield (x)).run assert_===("log1log2", 1)
    import std.vector._
    //MonadWriter[Writer, Vector[String]].point(1).run assert_===(Vector(), 1)
  }

  "Scalaz" should "\\/[+A, +B]" in {
    1.right[String].isRight assert_=== true
    1.right[String].isLeft assert_=== false
    1.right[String] | 0 assert_=== 1 // getOrElse
    //("boom".left ||| 2.right) assert_=== 2.right // orElse
   // ("boom".left[Int] >>= { x => (x + 1).right }) assert_=== "boom".left[Int]
   // (for {e1 <- 1.right; e2 <- "boom".left[Int]} yield (e1 |+| e2)) assert_=== "boom".left[Int]
  }

  "Scalaz" should "Kleisli[M[+_], -A, +B]" in {
    val k1 = Kleisli { (x: Int) => (x + 1).some }
    val k2 = Kleisli { (x: Int) => (x * 100).some }
    (4.some >>= k1 compose k2) assert_=== 401.some
    (4.some >>= k1 <=< k2) assert_=== 401.some
    (4.some >>= k1 andThen k2) assert_=== 500.some
    (4.some >>= k1 >=> k2) assert_=== 500.some
  }
  "Scalaz" should "Reader[E, A] = Kleisli[Id, E, A]" in {
    Reader {
      (_: Int) + 1
    }
  }

  "Scalaz" should "trait Memo[K, V]" in {
//    val memoizedFib: Int => Int = Memo.mutableHashMapMemo {
//      case 0 => 0
//      case 1 => 1
//      case n => memoizedFib(n - 2) + memoizedFib(n - 1)
//    }
  }

  "Scalaz" should "State[S, +A] = StateT[Id, S, A]" in {
    State[List[Int], Int] {
      case x :: xs => (xs, x)
    }.run(1 :: Nil) assert_===(Nil, 1)
    (for {
      xs <- get[List[Int]]
      _ <- put(xs.tail)
    } yield xs.head).run(1 :: Nil) assert_===(Nil, 1)
  }

  "Scalaz" should "ST[S, A]/STRef[S, A]/STArray[S, A]" in {
//    import effect._
//    import ST._
//    type ForallST[A] = Forall[({type l[x] = ST[x, A]})#l]
//    def e1[S]: ST[S, Int] = for {
//      x <- newVar[S](0)
//      _ <- x mod {
//        _ + 1
//      }
//      r <- x.read
//    } yield r
//    runST(new ForallST[Int] {
//      def apply[S] = e1[S]
//    }) assert_=== 1
//    def e2[S]: ST[S, ImmutableArray[Boolean]] = for {
//      arr <- newArr[S, Boolean](3, true)
//      x <- arr.read(0)
//      _ <- arr.write(0, !x)
//      r <- arr.freeze
//    } yield r
//    runST(new ForallST[ImmutableArray[Boolean]] {
//      def apply[S] = e2[S]
//    })(0) assert_=== false
  }

  "Scalaz" should "IO[+A]" in {
//    import effect._
//    import IO._
//    val action1 = for {
//      x <- readLn
//      _ <- putStrLn("Hello, " + x + "!")
//    } yield ()
//    action1.unsafePerformIO
  }

  "Scalaz" should "IterateeT[E, F[_], A]/EnumeratorT[O, I, F[_]]" in {
//    import iteratee._, Iteratee._
//    (length[Int, Id] &= enumerate(Stream(1, 2, 3))).run assert_=== 3
//    (length[scalaz.effect.IoExceptionOr[Char], IO] &= enumReader[IO](new BufferedReader(new FileReader("./README.md")))).run.unsafePerformIO
  }

  "Scalaz" should "Free[S[+_], +A]" in {
    import Free._
    type FreeMonoid[A] = Free[({type λ[+α] = (A, α)})#λ, Unit]
    // def cons[A](a: A): FreeMonoid[A] = Suspend[({type λ[+α] = (A,α)})#λ, Unit]((a, Return[({type λ[+α] = (A,α)})#λ, Unit](())))
//    def toList[A](list: FreeMonoid[A]): List[A] =
//      list.resume.fold(
//      { case (x: A, xs: FreeMonoid[A]) => x :: toList(xs) }, { _ => Nil })
//    toList(cons(1) >>= { _ => cons(2) }) assert_=== List(1, 2)
  }

  "Scalaz" should "Trampoline[+A] = Free[Function0, A]" in {
    import  Free._
    def even[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => return_(true)
        case x :: xs => suspend(odd(xs))
      }
    def odd[A](ns: List[A]): Trampoline[Boolean] =
      ns match {
        case Nil => return_(false)
        case x :: xs => suspend(even(xs))
      }
    even(0 |-> 3000).run assert_=== false
  }

  "Scalaz" should "Imports" in {
    import scalaz._ // imports type names
    import scalaz.Id.Id // imports Id type alias
    import scalaz.std.option._ // imports instances, converters, and functions related to `Option`
    import scalaz.std.AllInstances._ // imports instances and converters related to standard types
    import scalaz.std.AllFunctions._ // imports functions related to standard types
    import scalaz.syntax.monad._ // injects operators to Monad
    import scalaz.syntax.all._ // injects operators to all typeclasses and Scalaz data types
    import scalaz.syntax.std.boolean._ // injects operators to Boolean
    import scalaz.syntax.std.all._ // injects operators to all standard types
    import scalaz._, Scalaz._ // all the above
  }


  "Scalaz" should "Note" in {
//    type Function1Int[A] = ({type l[x] = Function1[Int, x]})#l[A]
//    type Function1Int[A] = Function1[Int, A]
  }
}
