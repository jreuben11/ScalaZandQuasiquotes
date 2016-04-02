package typeclasses

/**
  * Created by joshr on 01/04/2016.
  */
trait BasicTypeClass[A] {
  def doSomething(a: A): String
}


object BasicTypeClassClient {

  // function that implicitly uses a TypeClass
  def log[A](a: A)(implicit s: BasicTypeClass[A]) = println(s.doSomething(a))

  // implicit concrete-typed definition-registration of the TypeClass - could place in companion object and import
  implicit val stringTypeClass = new BasicTypeClass[String] {
    def doSomething(s: String) = s
  }

  // resolves the implicit implementation
  log("blah")
}
