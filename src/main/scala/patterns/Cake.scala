package patterns

/**
  * Created by joshr on 11/03/2016.
  */
trait AbstractMixin {def foo: String}
trait Mixin1 extends AbstractMixin {override def foo = "bar"}
trait Mixin2 extends AbstractMixin {override def foo = "baz"}
class Cake { this :AbstractMixin =>

}

object Blah {
  val x = new Cake with Mixin1
  val y = new Cake with Mixin2
}