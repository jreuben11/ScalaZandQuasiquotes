package reflect

import RuntimeTypetag._
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by joshr on 22/10/2015.
 */
class RuntimeTypetagTest extends FlatSpec with Matchers {
  "TypeTag" should "be able to reflect over an instance type" in {
    val l = List(1,2,3)
    val t = inspectRuntimeType(l)
    val symbols = t.decls //reflection
    t.toString should be ("List[Int]")
    inspectRuntimeType(new Cat).toString should be ("Cat")
    inspectRuntimeType(5).toString should be ("Int")
  }
  "TypeTag" should "be able to reflect over an instance type 2" in {
    val l = List(1,2,3)
    val t = inspectRuntimeType2(l)
    val symbols = t.decls //reflection
    t.toString should be ("List[Int]")
    inspectRuntimeType2(new Cat).toString should be ("Cat")
    inspectRuntimeType2(5).toString should be ("Int")
  }

  "TypeTag" should "be able to Instantiate a Class Type at Runtime" in {
    val params = List[Any]("blah", 0)
    val foo = instantiateType[Foo](params:_*)
    foo should be ( Foo("blah"))
  }

  "TypeTag" should "be able to increment a field at Runtime" in {
    val foo = Foo("blah")
    foo.count should be (0)
    incrementField(foo, "count")
    foo.count should be (1)
  }

  "TypeTag" should "be able to determine inheritence relations" in {
    val s = Seq.empty[Any]
    val l = List.empty[Any]
    isSubtype(l,s) should be (true)
    isSubtype(s,l) should be (false)
  }

  "TypeTag" should "be able to call a method at Runtime" in {
    val foo = Foo("blah")
    callMethod(foo, "bar") should be (foo.name)
  }
  "TypeTag" should "be able to resolve the instance of an Object Type at Runtime" in {
    resolveModule[Baz.type] should be (Baz)
  }


}
