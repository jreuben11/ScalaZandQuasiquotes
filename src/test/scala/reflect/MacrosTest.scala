package reflect

import Macros._

import org.scalatest.{FlatSpec, Matchers}
/**
 * Created by joshr on 10/3/15.
 */
class MacrosTest extends FlatSpec with Matchers {
  "Macros" should "get source code location of call" in{
    val l = getSourcecodeLocation
    l should be (SourcecodeLocation("/Users/joshr/IdeaProjects/scalaReflect/src/test/scala/reflect.MacrosTest.scala",8,13))
  }
}
