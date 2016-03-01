package commonAlgos

/**
  * Created by joshr on 01/03/2016.
  */
object LetterFrequency {
  import io.Source.fromFile

  def letterFrequencies(filename: String) =
    fromFile(filename).mkString groupBy (c => c) mapValues (_.length)
}
