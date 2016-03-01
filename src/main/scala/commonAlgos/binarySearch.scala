package commonAlgos

/**
  * Created by joshr on 01/03/2016.
  */
import scala.annotation.tailrec
object BinarySearch {

  def recursive[A <% Ordered[A]](a: IndexedSeq[A], v: A) = {
    @tailrec
    def recurse(low: Int, high: Int): Option[Int] = (low + high) / 2 match {
      case _ if high < low => None
      case mid if a(mid) > v => recurse(low, mid - 1)
      case mid if a(mid) < v => recurse(mid + 1, high)
      case mid => Some(mid)
    }
    recurse(0, a.size - 1)
  }

  def iterative[A <% Ordered[A]](xs: Seq[A], x: A): Option[Int] = {
    var (low, high) = (0, xs.size - 1)
    while (low <= high)
      (low + high) / 2 match {
        case mid if xs(mid) > x => high = mid - 1
        case mid if xs(mid) < x => low = mid + 1
        case mid => return Some(mid)
      }
    None
  }

}
