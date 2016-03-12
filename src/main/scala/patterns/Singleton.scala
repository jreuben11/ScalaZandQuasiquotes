package patterns

/**
  * Created by joshr on 25/02/2016.
  */
class Singleton private {}

object Singleton {
  private val singleton = new Singleton
  def apply = singleton
}
