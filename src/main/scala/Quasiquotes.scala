/**
 * Created by joshr on 10/8/15.
 */


//import scala.meta.dialects.Quasiquote - BAD !!!
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Quasiquotes {
  val toolbox: ToolBox[ru.type] = currentMirror.mkToolBox()

  def generateCode(): ru.Tree =
    q"package mypackage { class MyClass }"
  def saveToFile(path: String, code: Tree) = {
    val writer = new java.io.PrintWriter(path)
    try writer.write(showCode(code))
    finally writer.close()
  }
  def jit(): () => Any ={
    val code: ru.Tree = q"""println("compiled and run at runtime!")"""
    val compiledCode: () => Any = toolbox.compile(code)
    val result = compiledCode()
    compiledCode
  }
  def spliceAndUnlift(): List[Int] ={
    val x = 1
    val y = 2
    val z = 3
   val s = s"f($x, $y, $z)"

    val q"f(..${ints: List[Int]})" = q"f($x, $y, $z)"

   //val q"f(..${ints2: List[Int]})" = Quasiquote(StringContext(s))
    ints
  }
}

//object debug {
//  def apply[T](x: =>T): T = macro impl
//  def impl(c: Context)(x: c.Tree) = { import c.universe._
//    val q"..$stats" = x
//
//    val loggedStats = stats.flatMap { stat =>     //stats is Any ...
//      val msg = "executing " + showCode(stat)
//      List(q"println($msg)", stat)
//    }
//    q"..$loggedStats"
//  }
//}
