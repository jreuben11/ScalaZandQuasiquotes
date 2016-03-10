package commonAlgos

/**
  * Created by joshr on 10/03/2016.
  */
object Permutations {
  def permutations[T](list: List[T]):List[List[T]] = {
    list match {
      case Nil => Nil
      case elem :: Nil => List(list)
      case head :: tail => list.distinct.foldLeft(List[List[T]]())((lst, elem) => lst ++ ((permutations(list.diff(List(elem)))).map((l) => (elem :: l))))
    }
  }
}
