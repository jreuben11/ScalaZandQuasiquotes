package typeclasses

/**
  * Created by joshr on 03/04/2016.
  */

import cats.Functor
import cats.free.Free
import cats.free.Free.liftF
import cats.{Id, ~>}
import scala.collection.mutable


// Create an ADT representing your grammar
sealed trait KVStoreA[+Next]
case class Put[T, Next](key: String, value: T, next: Next) extends KVStoreA[Next]
case class Get[T, Next](key: String, onResult: T => Next) extends KVStoreA[Next]
case class Delete[Next](key: String, next: Next) extends KVStoreA[Next]


object FreeMonads {
  // Create a Free type based on your ADT
  type KVStore[A] = Free[KVStoreA, A]

  // Prove KVStoreA[_] has a Functor
  implicit val functor: Functor[KVStoreA] =
    new Functor[KVStoreA] {
      def map[A, B](kvs: KVStoreA[A])(f: A => B): KVStoreA[B] =
        kvs match {
          case Put(key, value, next) =>
            Put(key, value, f(next))
          case g: Get[t, A] => // help scalac with parametric type
            Get[t, B](g.key, g.onResult andThen f)
          case Delete(key, next) =>
            Delete(key, f(next))
         }
    }

  // Create smart constructors using liftF
  // Put returns nothing (i.e. Unit).
  def put[T](key: String, value: T): KVStore[Unit] =
    liftF(Put(key, value, ()))
  // Get returns a T value.
  def get[T](key: String): KVStore[T] =
    liftF(Get[T, T](key, identity))
  // Delete returns nothing (i.e. Unit).
  def delete(key: String): KVStore[Unit] =
    liftF(Delete(key, ()))
  // Update composes get and set, and returns nothing.
  def update[T](key: String, f: T => T): KVStore[Unit] =
    for {
      v <- get[T](key)
      _ <- put[T](key, f(v))
    } yield ()

  // Build a static program
  def program: KVStore[Int] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

  // Write a compiler for programs
  // a very simple (and imprecise) key-value store
  val kvs = mutable.Map.empty[String, Any]
  // the program will crash if a key is not found, or if a type is incorrectly specified.
  def impureCompiler =
    new (KVStoreA ~> Id) {
      def apply[A](fa: KVStoreA[A]): Id[A] =
          fa match {
            case Put(key, value, next) =>
              println(s"put($key, $value)")
              kvs(key) = value
              next
            case g: Get[t, A] =>
              println(s"get(${g.key})")
              g.onResult(kvs(g.key).asInstanceOf[t])
            case Delete(key, next) =>
              println(s"delete($key)")
              kvs.remove(key)
              next
          }
    }


  // Run the program
  val result: Id[Int] = program.foldMap(impureCompiler)

  // Pure computation
  def compilePure[A](program: KVStore[A], kvs: Map[String, A]): Map[String, A] =
    program.fold(_ => kvs,
      {
        case Put(key, value, next) => // help scalac past type erasure
          compilePure[A](next, kvs + (key -> value.asInstanceOf[A]))
        case g: Get[a, f] => // a bit more help for scalac
          compilePure(g.onResult(kvs(g.key).asInstanceOf[a]), kvs)
        case Delete(key, next) =>
          compilePure(next, kvs - key)
      })
  val result2: Map[String, Int] = compilePure(program, Map.empty)
}


