package reflect


import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

/**
 * Created by joshr on 22/10/2015.
 */
object Macros {
  def getSourcecodeLocation: SourcecodeLocation = macro getSourcecodeLocation_impl
  //must be public
  def getSourcecodeLocation_impl(context: Context): context.Expr[SourcecodeLocation] = {
    import context.universe._
    val position = context.macroApplication.pos
    val moduleSymbol = context.mirror.staticModule("reflect.SourcecodeLocation") // typename

    context.Expr(Apply(
      Ident(moduleSymbol),
      List(
        Literal(Constant(position.source.path)),
        Literal(Constant(position.line)),
        Literal(Constant(position.column))
      )
    ))
  }

//  def printf(format: String, params: Any*): Unit = macro printf_impl
//  def printf_impl(c: Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
//    val Literal(Constant(s_format: String)) = format.tree
//    val evals = ListBuffer[ValDef]()
//    def precompute(value: Tree, tpe: Type): Ident = {
//      val freshName = TermName(c.freshName("eval$"))
//      evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
//      Ident(freshName)
//    }
//    //TODO: c.universe.Tree to runtime.universe.Tree
//    val paramsStack = mutable.Stack[Tree]((params map (_.tree)): _*)
//    val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
//      case "%d" => precompute(paramsStack.pop, typeOf[Int])
//      case "%s" => precompute(paramsStack.pop, typeOf[String])
//      case "%%" => Literal(Constant("%"))
//      case part => Literal(Constant(part))
//    }
//
//    val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
//    c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
//  }
}

case class SourcecodeLocation(filename: String, line: Int, column: Int)
