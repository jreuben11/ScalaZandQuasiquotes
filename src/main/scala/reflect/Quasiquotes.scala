package reflect

/**
 * Created by joshr on 10/8/15.
 */


//import scala.meta.dialects.Quasiquote - BAD !!!
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{currentMirror, universe => ru}
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

  def typecheckType(tree: Tree): Type = toolbox.typecheck(tree, toolbox.TYPEmode).tpe

  def dynamic[T: TypeTag] (a:T, b:T): T = {
    val code = s"($a + $b): ${typeOf[T]}"
    val qq: toolbox.u.Tree = toolbox.parse(code) // eg q"(1 + 1): Int"
    val compiledCode: () => Any = toolbox.compile(qq)
    val result: Any = compiledCode()
    result.asInstanceOf[T]
  }

  def dynamic2[T: TypeTag] (a:T, op:String, b:T): T =  toolbox.compile(toolbox.parse(s"($a$op$b): ${typeOf[T]}"))().asInstanceOf[T]



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
