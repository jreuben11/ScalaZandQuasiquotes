package typeclasses

/**
  * Created by joshr on 03/04/2016.
  */

import cats.Functor
import cats.free.Free
import cats.free.Free.liftF

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

  // Build a program
  def program: KVStore[Int] =
    for {
      _ <- put("wild-cats", 2)
      _ <- update[Int]("wild-cats", (_ + 12))
      _ <- put("tame-cats", 5)
      n <- get[Int]("wild-cats")
      _ <- delete("tame-cats")
    } yield n

}


