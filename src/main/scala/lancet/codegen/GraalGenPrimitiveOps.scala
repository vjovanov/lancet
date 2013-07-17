package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenPrimitiveOps extends GraalGenBase with GraalBuilder {
  val IR: PrimitiveOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
//    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
//    case ObjDoublePositiveInfinity() => emitValDef(sym, "scala.Double.PositiveInfinity")
//    case ObjDoubleNegativeInfinity() => emitValDef(sym, "scala.Double.NegativeInfinity")
//    case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
//    case ObjDoubleMaxValue() => emitValDef(sym, "scala.Double.MaxValue")
//    case DoubleFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
//    case DoublePlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
//    case DoubleMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
//    case DoubleTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
//    case DoubleDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
//    case DoubleToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
//    case DoubleToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
//    case FloatToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
//    case FloatToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
//    case FloatPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
//    case FloatMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
//    case FloatTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
//    case FloatDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
//    case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
//    case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
//    case ObjIntMinValue() => emitValDef(sym, "scala.Int.MinValue")
    case IntPlus(lhs,rhs) =>
      insert(sym)
      push(rhs)
      push(lhs)
      frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(Kind.Int, lookup(sym))
    case IntMinus(lhs,rhs) =>
      insert(sym)
      push(rhs)
      push(lhs)
      frameState.push(Kind.Int, graph.unique(new IntegerSubNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(Kind.Int, lookup(sym))
    case IntTimes(lhs,rhs) =>
      insert(sym)
      push(rhs)
      push(lhs)
      frameState.push(Kind.Int, graph.unique(new IntegerMulNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(Kind.Int, lookup(sym))

//    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
//    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
//    case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
//    case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
//    case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
//    case IntBinaryXor(lhs,rhs) => emitValDef(sym, quote(lhs) + " ^ " + quote(rhs))
//    case IntShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
//    case IntShiftRightArith(lhs, rhs) => emitValDef(sym, quote(lhs) + " >> " + quote(rhs))
//    case IntShiftRightLogical(lhs, rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
//    case IntDoubleValue(lhs) => emitValDef(sym, quote(lhs) + ".doubleValue()")
//    case IntFloatValue(lhs) => emitValDef(sym, quote(lhs) + ".floatValue()")
//    case IntBitwiseNot(lhs) => emitValDef(sym, "~" + quote(lhs))
//    case IntToLong(lhs) => emitValDef(sym, quote(lhs) + ".toLong")
//    case IntToFloat(lhs) => emitValDef(sym, quote(lhs) + ".toFloat")
//    case IntToDouble(lhs) => emitValDef(sym, quote(lhs) + ".toDouble")
//    case LongBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
//    case LongBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
//    case LongShiftLeft(lhs,rhs) => emitValDef(sym, quote(lhs) + " << " + quote(rhs))
//    case LongShiftRightUnsigned(lhs,rhs) => emitValDef(sym, quote(lhs) + " >>> " + quote(rhs))
//    case LongToInt(lhs) => emitValDef(sym, quote(lhs) + ".toInt")
    case _ => super.emitNode(sym, rhs)
  }
}
