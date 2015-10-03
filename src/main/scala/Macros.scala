/**
 * Created by joshr on 10/3/15.
 */
import scala.language.experimental.macros // required to enable
import scala.reflect.macros.blackbox.Context

object Macros {
  def getSourcecodeLocation: SourcecodeLocation = macro getSourcecodeLocationImpl
  //must be public
  def getSourcecodeLocationImpl(context: Context): context.Expr[SourcecodeLocation] = {
    import context.universe._
    val position = context.macroApplication.pos
    val moduleSymbol = context.mirror.staticModule("SourcecodeLocation") // typename

    context.Expr(Apply(
      Ident(moduleSymbol),
      List(
        Literal(Constant(position.source.path)),
        Literal(Constant(position.line)),
        Literal(Constant(position.column))
      )
    ))
  }

}

case class SourcecodeLocation(filename: String, line: Int, column: Int)
