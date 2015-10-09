/**
 * Created by joshr on 10/6/15.
 */
import scala.tools.reflect.ToolBox  // see SBT
import scala.reflect.runtime.universe._

object RuntimeToolbox {
  val toolbox = runtimeMirror(getClass.getClassLoader).mkToolBox()

  def parseExpressionTree(code: String)   = {
    val tree: toolbox.u.Tree = toolbox.parse(code)
    tree
  }

  // why do I need this if i already have the Expr type ???
  def checkType()={
    val _expr: Expr[Int] = reify { "test".length }
    val _tree: Tree = _expr.tree
    val _typecheckedTree: toolbox.u.Tree = toolbox.typecheck(_tree)
    val t = _typecheckedTree.tpe
  }



}
