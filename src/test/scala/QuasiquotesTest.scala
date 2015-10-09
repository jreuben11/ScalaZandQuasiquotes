import org.scalatest.{Matchers, FlatSpec}
import Quasiquotes._
/**
 * Created by joshr on 10/8/15.
 */
  class QuasiquotesTest extends FlatSpec with Matchers {
  "Quasiquotes" should "run" in{
    val code = generateCode
    saveToFile("/tmp/gencode.scala", code)
    jit()
    spliceAndUnlift()
  }

}
