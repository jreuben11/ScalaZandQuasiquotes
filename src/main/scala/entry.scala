/**
 * Created by joshr on 10/2/15.
 */
object entry extends App {
  val l = List(1,2,3)
  val t = TypetagExamples.inspectRuntimeType(l)
  val typeName = t.tpe
  val x = TypetagExamples.inspectRuntimeTypeName(l)
  println(x)
  println("bye")
}
