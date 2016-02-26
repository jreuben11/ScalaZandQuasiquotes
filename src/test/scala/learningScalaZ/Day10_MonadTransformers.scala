//package learningScalaZ
//
///**
// * Created by joshr on 20/10/2015.
// */
//import org.scalatest.{Matchers, FlatSpec}
////import scala.language.higherKinds
////import scala.language.implicitConversions
////import scala.language.postfixOps
//
//import scalaz._
//import Scalaz._
//class Day10_MonadTransformers extends FlatSpec with Matchers {
//  "ScalaZ Reader" should "capture info instead of explicitly passing it arround" in {
//    def myName(step: String): Reader[String, String] = Reader {
//      step + ", I am " + _
//    }
//    def localExample: Reader[String, (String, String, String)] = for {
//      a <- myName("First")
//      b <- myName("Second") >=> Reader {
//        _ + "dy"
//      }
//      c <- myName("Third")
//    } yield (a, b, c)
//    localExample("Fred").toString() should be("(First, I am Fred,Second, I am Freddy,Third, I am Fred)")
//
//  }
//
//  "ScalaZ Reader" should "stacking ReaderT, monad transformer version of Reader on Option monad " +
//    "convert a Map to an Option of Tuple3 - using A CUSTOM TYPE Map[String, String] as a reader" in {
//    type ReaderTOption[A, B] = ReaderT[Option, A, B]
//    object ReaderTOption extends KleisliInstances  {
//      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
//    }
//    def configure(key: String) = ReaderTOption[Map[String, String], String] {_.get(key)}
//
//    def setupConnection = for {
//      host <- configure("host")
//      user <- configure("user")
//      password <- configure("password")
//    } yield (host, user, password)
//
//    val goodConfig = Map(
//      "host" -> "eed3si9n.com",
//      "user" -> "sa",
//      "password" -> "****"
//    )
//
//    setupConnection(goodConfig) should be(("eed3si9n.com", "sa", "****").some)
//
//  }
//
//  "ScalaZ Reader" should "Stack multiple monad transformers - revolting" in {
//    type ReaderTOption[A, B] = ReaderT[Option, A, B]
//    object ReaderTOption extends KleisliInstances  {
//      def apply[A, B](f: A => Option[B]): ReaderTOption[A, B] = Kleisli(f)
//    }
//
//    type StateTReaderTOption[C, S, A] = StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A]
//
//    object StateTReaderTOption extends StateTInstances with StateTFunctions {
////      def apply[C, S, A](f: S => (S, A)) = new StateT[({type l[X] = ReaderTOption[C, X]})#l, S, A] {
////        def apply(s: S) = f(s).point[({type l[X] = ReaderTOption[C, X]})#l]
////      }
////      def get[C, S]: StateTReaderTOption[C, S, S] =
////        StateTReaderTOption { s => (s, s) }
////      def put[C, S](s: S): StateTReaderTOption[C, S, Unit] =
////        StateTReaderTOption { _ => (s, ()) }
//    }
//
//    type Stack = List[Int]
//    type Config = Map[String, String]
//    val pop: StateTReaderTOption[Config, Stack, Int] = {
//      import StateTReaderTOption.{get, put}
//      for {
//        s <- get[Config, Stack]
//        (x :: xs) = s
//        _ <- put(xs)
//      } yield x
//    }
////    val pop1 = StateTReaderTOption[Config, Stack, Int] {
////      case x :: xs => (xs, x)
////    }
//    def push(x: Int): StateTReaderTOption[Config, Stack, Unit] = {
//      import StateTReaderTOption.{get, put}
//      for {
//        xs <- get[Config, Stack]
//        r <- put(x :: xs)
//      } yield r
//    }
//
//    def stackManip: StateTReaderTOption[Config, Stack, Int] = for {
//      _ <- push(3)
//      a <- pop
//      b <- pop
//    } yield(b)
//    //stackManip(List(5, 8, 2, 1))(Map()) should be (Some((List(8, 2, 1),5)))
//
////    def configure[S](key: String) = new StateTReaderTOption[Config, S, String] {
////      def apply(s: S) = ReaderTOption[Config, (S, String)] { config: Config => config.get(key) map {(s, _)} }
////    }
////    def stackManip2: StateTReaderTOption[Config, Stack, Unit] = for {
////      x <- configure("x")
////      a <- push(x.toInt)
////    } yield(a)
//    // BROKE in 7.2.0: type mismatch;
//   // found   : scala.collection.immutable.Map[String,String]
//    // required: scalaz.Monad[[X]scalaz.Kleisli[Option,Config,X]]
//   // (which expands to)  scalaz.Monad[[X]scalaz.Kleisli[Option,scala.collection.immutable.Map[String,String],X]]
//    val kliesliMap = Kleisli(Map("x" -> "7"))
////    stackManip2(List(5, 8, 2, 1))(kliesliMap) should be (Some((List(7, 5, 8, 2, 1),())))
////    stackManip2(List(5, 8, 2, 1))(kliesliMap) should be (None)
//
//
//
//  }
//
//}
//
//
