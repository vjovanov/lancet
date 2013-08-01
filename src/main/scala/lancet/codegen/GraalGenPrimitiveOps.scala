package lancet.codegen

import java.lang.reflect.Modifier._

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

    //Double
    case DoublePlus(lhs,rhs)   =>
      operation(sym)(x => append(graph.unique(new FloatAddNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case DoubleMinus(lhs,rhs)  =>
      operation(sym)(x => append(graph.unique(new FloatSubNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case DoubleTimes(lhs,rhs)  =>
      operation(sym)(x => append(graph.unique(new FloatMulNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case DoubleDivide(lhs,rhs) =>
      operation(sym)(x => append(graph.unique(new FloatRemNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))

    // Floating Point
    case FloatPlus(lhs,rhs)    =>
      operation(sym)(x => append(graph.unique(new FloatAddNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case FloatMinus(lhs,rhs)   =>
      operation(sym)(x => append(graph.unique(new FloatSubNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case FloatTimes(lhs,rhs)   =>
      operation(sym)(x => append(graph.unique(new FloatMulNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))
    case FloatDivide(lhs,rhs)  =>
      operation(sym)(x => append(graph.unique(new FloatRemNode(Kind.Int, x(0), x(1), isStrict(method.getModifiers())))))

    // Intenger
    case IntPlus(lhs,rhs)      =>
      operation(sym)(x => graph.unique(new IntegerAddNode(Kind.Int, x(0), x(1))))
    case IntMinus(lhs,rhs)     =>
      operation(sym)(x => graph.unique(new IntegerSubNode(Kind.Int, x(0), x(1))))
    case IntTimes(lhs,rhs)     =>
      operation(sym)(x => graph.unique(new IntegerMulNode(Kind.Int, x(0), x(1))))
    case IntDivide(lhs,rhs)    =>
      operation(sym)(x => graph.add(append(new IntegerDivNode(Kind.Int, x(0), x(1)))))
    case IntMod(lhs,rhs)       =>
      operation(sym)(x => graph.add(append(new IntegerRemNode(Kind.Int, x(0), x(1)))))

    // Conversions
    case IntToLong(lhs) =>
      insert(sym)
      push(lhs)
      genConvert(ConvertNode.Op.I2L)
      storeLocal(kind(sym), lookup(sym))
    case IntToFloat(lhs) =>
      insert(sym)
      push(lhs)
      genConvert(ConvertNode.Op.I2F)
      storeLocal(kind(sym), lookup(sym))
    case IntToDouble(lhs) =>
      insert(sym)
      push(lhs)
      genConvert(ConvertNode.Op.I2D)
      storeLocal(kind(sym), lookup(sym))
    case FloatToInt(lhs) =>
      insert(sym)
      push(lhs)
      genConvert(ConvertNode.Op.F2I)
      storeLocal(kind(sym), lookup(sym))
    case FloatToDouble(lhs) =>
      insert(sym)
      push(lhs)
      genConvert(ConvertNode.Op.F2D)
      storeLocal(kind(sym), lookup(sym))


    case _ => super.emitNode(sym, rhs)
  }
}
