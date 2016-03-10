package commonAlgos

import scala.collection.SortedMap

/**
  * Created by joshr on 10/03/2016.
  */
object Stats {
  def mean(a:Array[Double])=a.sum / a.size
  def stddev(a:Array[Double])={
    val sum = a.fold(0.0)((a, b) => a + math.pow(b,2))
    math.sqrt((sum/a.size) - math.pow(mean(a),2))
  }
  def hist(a:Array[Double]) = {
    val grouped=(SortedMap[Double, Array[Double]]() ++ (a groupBy (x => math.rint(x*10)/10)))
    grouped.map(v => (v._1, v._2.size))
  }
}
