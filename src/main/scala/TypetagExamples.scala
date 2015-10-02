/**
 * Created by joshr on 10/2/15.
 */


import scala.reflect.runtime.universe.{TypeTag,typeTag, Type} //from scala-reflect
object TypetagExamples {
  def inspectRuntimeType[T: TypeTag](obj: T) = typeTag[T]
  def inspectRuntimeTypeName[T: TypeTag](obj: T): Type = typeTag[T].tpe
}
