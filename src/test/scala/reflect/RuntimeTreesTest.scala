package reflect

/**
 * Created by joshr on 10/3/15.
 */

import RuntimeToolbox._
import RuntimeTrees._
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.universe._
class RuntimeTreesTest extends FlatSpec with Matchers {
  "RuntimeTrees" should "be able to construct expression trees" in {
    show(buildExpressionTree("x", "plus", 1)) should be  ("x.$plus(1)")
    show(buildExpressionTree("x", "plus", 5)) should be  ("x.$plus(5)")
    val (func, arg) = buildExpressionTree("x","plus", 1) match {
      case Apply(fn, a :: Nil) => (fn, a)
    }
    func.toString() should be ("x.$plus")
    arg.toString() should be (1.toString)
  }

  "RuntimeTrees" should "be able to show reified class expressions" in {

    val expr: Expr[Unit] = reify { class Flower { def name = "Rose" } }
    showExpressionTree(expr) should be (""" {
                                      |  class Flower extends AnyRef {
                                      |    def <init>() = {
                                      |      super.<init>();
                                      |      ()
                                      |    };
                                      |    def name = "Rose"
                                      |  };
                                      |  ()
                                      |} """.stripMargin.trim)

    val rawExpr = """
      Block(List(ClassDef(Modifiers(), TypeName("Flower"), List(), Template(List(Ident(TypeName("AnyRef"))), noSelfType, List(DefDef(Modifiers(), termNames.CONSTRUCTOR, List(), List(List()), TypeTree(), Block(List(Apply(Select(Super(This(typeNames.EMPTY), typeNames.EMPTY), termNames.CONSTRUCTOR), List())), Literal(Constant(())))), DefDef(Modifiers(), TermName("name"), List(), List(), TypeTree(), Literal(Constant("Rose"))))))), Literal(Constant(())))
    """.stripMargin.trim
    showRawExpressionTree(expr) should be (rawExpr)
  }

  "RuntimeToolbox" should "be able to parse expression trees" in {
    val (func, args) = parseExpressionTree("println(1)") match {
      case Apply(fn, a :: Nil) => (fn, a)
    }
    func.toString() should be ("println")
    args.toString() should be (1.toString)
  }
}

