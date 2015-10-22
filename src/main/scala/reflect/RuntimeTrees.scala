package reflect

/**
 * Created by joshr on 10/3/15.
 */
import scala.reflect.runtime.universe._

object RuntimeTrees {
  def buildExpressionTree(operandName: String, operatorName: String, value: Integer) = {
    val tree: Apply = Apply(
      Select(
        Ident(
          TermName(operandName)
        ),
        TermName("$" + operatorName)
      ),
      List(
        Literal(
          Constant(value)
        )
      )
    )
    tree
  }


  def showExpressionTree(expr: Expr[Unit]) = expr.tree.toString()

  def showRawExpressionTree(expr: Expr[Unit]): String = {
    val tree: Tree = expr.tree
    val rawTree: String = showRaw(tree)
    rawTree
  }
}


