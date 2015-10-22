package reflect

import Quasiquotes._
import org.scalatest.{FlatSpec, Matchers}

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}
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

  "Quasiquotes" should "convert code to string" in {
    val tree: Tree = q"class C"
    showCode(tree) should be ("class C")

    showRaw(q"List[Int]")  should be ("""TypeApply(Ident(TermName("List")), List(Ident(TypeName("Int"))))""")
    showRaw(tq"List[Int]") should be ("""AppliedTypeTree(Ident(TypeName("List")), List(Ident(TypeName("Int"))))""")
  }


//  "Quasiquotes" should "match a case" in {
//    val qq1 = q"i am { a quasiquote }"
//    val qq2 = q"i am { a quasiquote }"
//    val qq3 = q"i am { an quasiquote }"
// Warning:(46, 10) patterns after a variable pattern cannot match (SLS 8.1.1)
// If you intended to match against parameter tree2 of method matchCase, you must use backticks, like: case `tree2` => case tree2 => true
//def matchCase(tree1:Tree, tree2:Tree) = tree1 match {
//  case tree2 => true
//  case _ => false
//}
//    matchCase(qq1, qq2) should be (true)
//    matchCase(qq1, qq3) should be (false)
//  }

  "Quasiquotes" should "evaluate tree structures for equality" in {
    val tree1 = q"foo + bar"
    val tree2 = q"foo.+(bar)"
    tree1 equalsStructure tree2 should be (true)
  }

  "Quasiquotes" should "use unquote to structurally substitute subtrees" in {
    val tree1 = q"a quasiquote"
    val tree2 = q"i am { $tree1 }"
    val tree3 = q"i.am(a.quasiquote)"
    tree2 equalsStructure tree3 should be (true)
  }
  "Quasiquotes" should "use unquote to deconstruct trees" in {
    val tree1 = q"a quasiquote"
    val tree2 = q"i am { $tree1 }"
    val q"i am $tree3" = tree2
    tree1 equalsStructure tree3 should be (true)
  }
  "Quasiquotes" should "interpolate" in {
    val tree1 =
      q"""
         val x: List[Int] = List(1, 2) match {
           case List(a, b) => List(a + b)
         }
       """.asInstanceOf[ValDef]
//    val compiledCode: () => Any = toolbox.compile(tree)
//    val result = compiledCode()
//    result should be (1)    // <(), the Unit value> was not equal to 1
    val m = tree1.rhs.asInstanceOf[Match]
    m.selector equalsStructure q"List(1,2)" should be (true)
    val caseDef = m.cases(0)

    showCode(caseDef.pat) should be ("List((a @ _), (b @ _))")    // could not qq @
    caseDef.body  equalsStructure q"List(a.+(b))" should be (true)
  }

  "Quasiquotes" should "splice" in {
    val ab = List(q"a", q"b")
    val fab = q"f(..$ab)"
    fab equalsStructure q"f(a,b)" should be (true)
    val c = q"c"
    val fabc = q"f(..$ab, $c)"
    fabc equalsStructure q"f(a,b,c)" should be (true)
    val fabcab = q"f(..$ab, $c, ..$ab)"
    fabcab equalsStructure q"f(a,b,c,a,b)" should be (true)
    val argss = List(ab, List(c))
    val fargss = q"f(...$argss)"
    fargss equalsStructure q"f(a, b)(c)" should be (true)

    val q"f(..$args)" = q"f(a, b)"
    val ab2 = args.asInstanceOf[List[Ident]]
    ab2(0) equalsStructure ab(0) should be (true)
    ab2(1) equalsStructure ab(1) should be (true)
  }

  "Quasiquotes" should "lift" in {
    val two = 1 + 1
    val four = q"$two + $two"
    four equalsStructure q"2.+(2)" should be (true)

    val ints = List(1, 2, 3)
    val f123 = q"f(..$ints)"
    f123 equalsStructure q"f(1,2,3)" should be (true)

    val intss = List(List(1, 2, 3), List(4, 5), List(6))
    val f123456 = q"f(...$intss)"
    f123456  equalsStructure q"f(1,2,3)(4,5)(6)" should be (true)

  }

  "Quasiquotes" should "lift custom Liftables" in {
    //package object points{}
    case class Point(x: Int, y: Int)
    object Point {
      implicit val lift = Liftable[Point] { p =>
        q"_root_.points.Point(${p.x}, ${p.y})"
      }
      implicit val unliftPoint = Unliftable[Point] {
        case q"_root_.points.Point(${x: Int}, ${y: Int})" => Point(x, y)
      }
    }
  }

  "Quasiquotes" should "support typed unlifting" in {
    val q"${left: Int} + ${right: Int}" = q"2 + 2"
    left + right should be (4)
  }

  "Quasiquotes" should "support type checking" in {
    typecheckType(tq"Map[_, _]") =:= typeOf[Map[_, _]] should be (true)
    typecheckType(tq"Map[_, _]") =:= typeOf[collection.immutable.Map[_, _]] should be (true)
  }

  "Quasiquotes" should "Dynamic" in {
    dynamic(1,2) should be (3)
    dynamic2(1,"+",2) should be (3)
  }


}
