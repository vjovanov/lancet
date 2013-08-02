package lancet.codegen

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

object Conversions {
  final def l2s(l: Long): String = _root_.java.lang.Long.toString(l)
  final def i2s(i: Int): String = _root_.java.lang.Integer.toString(i)
  final def d2s(d: Double): String = _root_.java.lang.Double.toString(d)
  final def f2s(f: Float): String = _root_.java.lang.Float.toString(f)
  final def b2s(b: Boolean): String = _root_.java.lang.Boolean.toString(b)
}

trait GraalGenMiscOps extends GraalNestedCodegen with GraalBuilder {
  val IR: MiscOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case PrintF(f,x) => ???//emitValDef(sym, "printf(" + (f::x.map(quote)).mkString(",") + ")")
      case PrintLn(s)  =>
        insert(sym)
        push(Const(Predef))
        pushToString(s)
        invoke(Predef.getClass, "println", classOf[Any])
      case Print(s)    =>
        insert(sym)
        push(Const(Predef))
        pushToString(s)
        invoke(Predef.getClass, "print", classOf[Any])
      case Exit(a)     => ???//emitValDef(sym, "exit(" + quote(a) + ")")
      case Return(x)   => ???//emitValDef(sym, "return " + quote(x))
      case Error(s)    => ???//emitValDef(sym, "error(" + quote(s) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}