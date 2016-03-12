package patterns

/**
  * Created by joshr on 12/03/2016.
  */
object ExtensionMethods {
  import Extensions._
  val s = "xxx".doXXX
  val a = A("yyy").doYYY
}

object Extensions {
  implicit class StringEx(val s: String) {
    def doXXX = s.reverse.toUpperCase
  }
  implicit class AEx(val a: A) {
    def doYYY = a.copy(s = a.s.reverse.toUpperCase)
  }
}
case class A (s: String)
