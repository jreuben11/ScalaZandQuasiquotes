/**
 * Created by joshr on 10/2/15.
 */
import scalaz._
import Scalaz._
object entry extends App {
 // (Tags.Conjunction(true) |+| Tags.Conjunction(false)) assert_=== false
  import scalaz.std.anyVal.intInstance
  import scalaz.std.int._
  import scalaz.syntax.enum._
  import scalaz.syntax.monoid._
 //import scalaz.syntax.foldable._

//  display("anyVal.intInstance is Monoid, Enum (Enum is also Order and Order is Equal), Show")
//
//  val x: (Int, String) = (heaviside(1), """ heaviside(1) """)
//  display(heaviside(-1), """ heaviside(-1) """)
//
//  display("MonoidOps")
//  display(1 ⊹ 1, """ 1 ⊹ 1 """)
//  display(1 ⊹ ∅, """ 1 ⊹ ∅ """)
//
//  display("EnumOps")
//  display(1.succ, """ 1.succ """)
//  display(1 -+- 2, """ 1 -+- 2 """)
//  display(1.pred, """ 1.pred """)
//  display(1 --- 2, """ 1 -+- 2 """)
//  display(1 |-> 10, """ 1 |-> 10 """)
//  display(1 |--> (2, 10), """ 1 |--> (2, 10) """)
//  println(1 |--> (2, 11), """ 1 |--> (2, 11) """)
//
//  display("ShowOps")
//  display(1.shows, """ 1.show """)
//  1.println
//
//  display("EqualOps")
//  display(1 ≟ 1, """ 1 ≟ 1 """)
//  display(1 ≠ 2, """ 1 ≠ 2 """)
//
//  display("OrderOps")
//  display(1 lt 1, """ 1 lt 2 """)
//  display(2 lte 2, """ 2 lte 2 """)
//  display(2 gt 1, """ 2 gt 1 """)
//  display(2 gte 2, """ 2 gte 2 """)
//  display(1 ?|? 2, """ 1 ?|? 2 """)
//  display(1 max 2, """ 1 max 2 """)
//  display(1 min 2, """ 1 min 2 """)
  println("bye")
}
