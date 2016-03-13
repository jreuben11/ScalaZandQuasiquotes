package patterns

/**
  * Created by joshr on 13/03/2016.
  */
object Bounds {
//  class Container[A <% Int] { def addIt(x: A) = 123 + x }
//  (new Container[Int]).addIt(123)
//  (new Container[String]).addIt("123")  // works because of String.toInt


  def viewBound[A <% Ordered[A]](a: A, b: A) = if (a < b) a else b
  def viewBound2[A](a: A, b: A)(implicit imp: A => Ordered[A]) = if (a < b) a else b

  def contextBound[A : Ordering](a: A, b: A) = implicitly[Ordering[A]].compare(a, b)
  def contextBound2[A : Ordering](a: A, b: A) (implicit imp: Ordering[A]) = imp.compare(a, b)

  viewBound(1,2)
  contextBound(1,2)
  contextBound2(1,2)
}
