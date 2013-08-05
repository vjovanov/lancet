package lancet.codegen

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

object Math {
  final def abs(d: Double): Double = _root_.java.lang.Math.abs(d)
}

trait GraalGenMathOps extends GraalNestedCodegen with GraalBuilder {
  val IR: MathOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match { // TODO: use java.lang.Math etc...
    case MathCeil(x)    => ??? // emitValDef(sym, "java.lang.Math.ceil(" + quote(x) + ")")
    case MathFloor(x)   => ??? // emitValDef(sym, "java.lang.Math.floor(" + quote(x) + ")")
    case MathExp(x)     => ??? // emitValDef(sym, "java.lang.Math.exp(" + quote(x) + ")")
    case MathLog(x)     => ??? // emitValDef(sym, "java.lang.Math.log(" + quote(x) + ")")
    case MathLog10(x)   => ??? // emitValDef(sym, "java.lang.Math.log10(" + quote(x) + ")")
    case MathSqrt(x)    => ??? // emitValDef(sym, "java.lang.Math.sqrt(" + quote(x) + ")")
    case MathSin(x)     => ??? // emitValDef(sym, "java.lang.Math.sin(" + quote(x) + ")")
    case MathSinh(x)    => ??? // emitValDef(sym, "java.lang.Math.sinh(" + quote(x) + ")")
    case MathAsin(x)    => ??? // emitValDef(sym, "java.lang.Math.asin(" + quote(x) + ")")
    case MathCos(x)     => ??? // emitValDef(sym, "java.lang.Math.cos(" + quote(x) + ")")
    case MathCosh(x)    => ??? // emitValDef(sym, "java.lang.Math.cosh(" + quote(x) + ")")
    case MathAcos(x)    => ??? // emitValDef(sym, "java.lang.Math.acos(" + quote(x) + ")")
    case MathTan(x)     => ??? // emitValDef(sym, "java.lang.Math.tan(" + quote(x) + ")")
    case MathTanh(x)    => ??? // emitValDef(sym, "java.lang.Math.tanh(" + quote(x) + ")")
    case MathAtan(x)    => ??? // emitValDef(sym, "java.lang.Math.atan(" + quote(x) + ")")
    case MathAtan2(x,y) => ??? // emitValDef(sym, "java.lang.Math.atan2(" + quote(x) + ", " + quote(y) + ")")
    case MathPow(x,y)   => ??? // emitValDef(sym, "java.lang.Math.pow(" + quote(x) + "," + quote(y) + ")")
    case MathAbs(x)     =>
      insert(sym)
      push(Const(lancet.codegen.Math))
      push(x)
      invoke(lancet.codegen.Math.getClass, "abs", x.tp.runtimeClass)
      storeLocal(kind(sym), lookup(sym))
    case MathMax(x,y)   => ??? // emitValDef(sym, "java.lang.Math.max(" + quote(x) + ", " + quote(y) + ")")
    case MathMin(x,y)   => ??? // emitValDef(sym, "java.lang.Math.min(" + quote(x) + ", " + quote(y) + ")")
    case MathPi()       => ??? // emitValDef(sym, "java.lang.Math.PI")
    case MathE()        => ??? // emitValDef(sym, "java.lang.Math.E")
    case _ => super.emitNode(sym, rhs)
  }
}
