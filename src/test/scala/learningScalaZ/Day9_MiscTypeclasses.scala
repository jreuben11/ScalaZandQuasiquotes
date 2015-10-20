package learningScalaZ

/**
 * Created by joshr on 20/10/2015.
 */
import org.scalatest.{Matchers, FlatSpec}
//import scala.language.higherKinds
//import scala.language.implicitConversions
//import scala.language.postfixOps

import scalaz._
import Scalaz._

class Day9_MiscTypeclasses extends FlatSpec with Matchers {
   "ScalaZ Tree" should "navigate trees" in {
     def freeTree: Tree[Char] = 'P'.node(
       'O'.node(
         'L'.node('N'.leaf, 'T'.leaf),
         'Y'.node('S'.leaf, 'A'.leaf)),
       'L'.node(
         'W'.node('C'.leaf, 'R'.leaf),
         'A'.node('A'.leaf, 'C'.leaf)))

     val treeloc: TreeLoc[Char] = freeTree.loc

     (treeloc.getChild(2) >>= {_.getChild(1)}).get.getLabel.some should be( Some('W'))
   }

   "ScalaZ Zipper" should "navigate streams" in {
     val zip: Option[Zipper[Int]] = Stream(1, 2, 3, 4).toZipper
     zip.get.focus should be (1)
     (zip >>= {_.next}).get.focus should be (2)
     (zip >>= {_.next} >>= {_.next}).get.focus should be (3)
     (zip >>= {_.next} >>= {_.previous}).get.focus should be (1)
     val zip2 = zip >>= {_.next} >>= {_.next} >>= {_.modify {_ => 7}.some}
     zip2.get.toStream.toList should be (List(1, 2, 7, 4))
     val zip3 = for {
       z <- Stream(1, 2, 3, 4).toZipper
       n1 <- z.next
       n2 <- n1.next
     } yield { n2.modify {_ => 7} }
     zip3 should be (zip2)
   }

   "ScalaZ Id" should "apply a function to an expression, visit an expression with a function" in {
     (0: Id[Int]) should be (0)
     // |> lets you write the function application at the end of an expression
     1 + 2 + 3 |> {_.point[List]} should be (List(6))
     1 + 2 + 3 |> {_ * 6} should be (36)
     1 visit { case x@(2|3) => List(x * 2) } should be (List(1))
     2 visit { case x@(2|3) => List(x * 2) } should be (List(4))
   }



 }
