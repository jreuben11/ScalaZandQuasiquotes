package typeclasses

import cats._

/**
  * Created by joshr on 02/04/2016.
  */
object ApplyTypeClass {
  implicit val optionApply: Apply[Option] = new Apply[Option] {
    override def map[A,B](fa: Option[A])(f: A => B) = fa map f
    override def ap[A, B](ff: Option[(A) => B])(fa: Option[A]): Option[B] = fa.flatMap (a => ff.map (ff => ff(a)))
    override def product[A, B](fa: Option[A], fb: Option[B]): Option[(A, B)] = ???
  }
  implicit val listApply: Apply[List] = new Apply[List] {
    override def map[A,B](fa: List[A])(f: A => B) = fa map f
    override def ap[A, B](ff: List[(A) => B])(fa: List[A]): List[B] = fa.flatMap (a => ff.map (ff => ff(a)))
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = ???
  }
}

