/**
 * Created by joshr on 10/2/15.
 */
import org.scalatest._
import TypetagExamples._

class TypetagExampleTest extends FlatSpec with Matchers {
  "a blah" should "blah" in {
    val l = List(1,2,3)
    val t = inspectRuntimeType(l)
    inspectRuntimeTypeName(l).toString should be ("List[Int]")

  }
}
