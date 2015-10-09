/**
 * Created by joshr on 10/2/15.
 */

//from scala-reflect
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
object RuntimeTypetag {

  def inspectRuntimeType[T: TypeTag](o: T): Type = typeTag[T].tpe
  // same:
  def inspectRuntimeType2[T](o: T)(implicit tag: TypeTag[T]): Type = tag.tpe

  def resolveModule[T: TypeTag]():T ={
    val t: Type = typeOf[T]
    val _mirror: Mirror = runtimeMirror(getClass.getClassLoader)
    val _moduleSymbol: ModuleSymbol = t.termSymbol.asModule
    val _moduleMirror: ModuleMirror = _mirror.reflectModule(_moduleSymbol)
    _moduleMirror.instance.asInstanceOf[T]

  }
  def instantiateType[T: TypeTag](args: Any*):T ={
    val t: Type = typeOf[T]
    val _mirror : Mirror = runtimeMirror(getClass.getClassLoader)
    val _classSymbol: ClassSymbol = t.typeSymbol.asClass
    val _classMirror: ClassMirror = _mirror.reflectClass(_classSymbol)
    val _ctorSymbol: MethodSymbol = t.decl(termNames.CONSTRUCTOR).asMethod
    val _ctorMirror: MethodMirror = _classMirror.reflectConstructor(_ctorSymbol)
    val o = _ctorMirror(args(0), args(1)).asInstanceOf[T]  // TODO: param array
    o
  }

  def incrementField[T: TypeTag](o: T, termName: String): Unit = {
    val t: Type = typeOf[T]
    val _mirror = runtimeMirror(o.getClass.getClassLoader)
    val _termSymbol = t.decl(TermName(termName)).asTerm
    implicit val c = typeToClassTag[T]  //Note: not in doco
    val _instanceMirror = _mirror.reflect(o)
    val _fieldMirror = _instanceMirror.reflectField(_termSymbol)
    _fieldMirror.set(_fieldMirror.get.asInstanceOf[Int] + 1)
  }

  def isSubtype[T: TypeTag, S: TypeTag](x: T, y: S): Boolean = typeTag[T].tpe <:< typeTag[S].tpe

  def callMethod[T: TypeTag](o: T, termName: String): String ={
    val t: Type = typeOf[T]
    val _mirror = runtimeMirror(o.getClass.getClassLoader)
    val _methodSymbol = t.decl(TermName(termName)).asMethod
    implicit val c = typeToClassTag[T]  //Note: not in doco
    val _instanceMirror = _mirror.reflect(o)
    val _methodMirror = _instanceMirror.reflectMethod(_methodSymbol)
    _methodMirror().asInstanceOf[String]  //TODO: reflect on return type
  }

  private def typeToClassTag[T: TypeTag]: ClassTag[T] = {
    ClassTag[T]( typeTag[T].mirror.runtimeClass( typeTag[T].tpe ) )
  }
}


