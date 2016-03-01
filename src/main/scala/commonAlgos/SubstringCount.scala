package commonAlgos

/**
  * Created by joshr on 01/03/2016.
  */
object SubstringCount {

  import scala.annotation.tailrec
  def recursive(str1:String, str2:String):Int={
    @tailrec def count(pos:Int, c:Int):Int={
      val idx=str1 indexOf(str2, pos)
      if(idx == -1) c else count(idx+str2.size, c+1)
    }
    count(0,0)
  }

  def regex( str:String, substr:String ) = substr.r.findAllMatchIn(str).length


}
