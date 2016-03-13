package patterns

/**
  * Created by joshr on 12/03/2016.
  */
object PartialFunctions {
  val pf : PartialFunction[Int,Int] = {
    case i: Int if (i >= 0) => i + 1
  }
  pf.lift(0) // Some(1)
  pf.lift(-1) // None

  (-5 to 5).toList.collect(pf)

  pf.isDefinedAt(0) // true
  pf.isDefinedAt(-1) // false

  val pf2: PartialFunction[Int, Int] = pf orElse {case -1 => 0 }

}

object AddOne extends Function1[Int, Int] {
  def apply(m: Int): Int = m + 1
}


