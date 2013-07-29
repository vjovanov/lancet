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
    case IntPlus(lhs,rhs) =>
      insert(sym)
      push(rhs, lhs)
      val addLhs = frameState.pop(Kind.Int)
      val addRhs = frameState.pop(Kind.Int)
      frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, addLhs, addRhs)))
      storeLocal(Kind.Int, lookup(sym))
    case IntMinus(lhs,rhs) =>
      insert(sym)
      push(rhs, lhs)
      frameState.push(Kind.Int, graph.unique(new IntegerSubNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(Kind.Int, lookup(sym))
    case IntTimes(lhs,rhs) =>
      insert(sym)
      push(rhs, lhs)
      frameState.push(Kind.Int, graph.unique(new IntegerMulNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(Kind.Int, lookup(sym))

//    // case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) =>
      insert(sym)
      push(rhs, lhs)
      frameState.push(Kind.Int, graph.add(append(new IntegerDivNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int)))))
      storeLocal(Kind.Int, lookup(sym))
    case IntMod(lhs,rhs) =>
      insert(sym)
      push(rhs, lhs)
      frameState.push(Kind.Int, graph.add(append(new IntegerRemNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int)))))
      storeLocal(Kind.Int, lookup(sym))
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
>>>>>>> eced20d06128db90ec9a0c6c9497871690c685e9
    case _ => super.emitNode(sym, rhs)
  }
}
