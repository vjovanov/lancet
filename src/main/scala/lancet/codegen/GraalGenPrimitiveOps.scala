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
      operation(sym)(x => append(new FloatAddNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))))
    case DoubleMinus(lhs,rhs)  =>
      operation(sym)(x => append(new FloatSubNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))))
    case DoubleTimes(lhs,rhs)  =>
      operation(sym)(x => append(new FloatMulNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))))
    case DoubleDivide(lhs,rhs) =>
      operation(sym)(x => append(new FloatDivNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))))

    // Floating Point
    case FloatPlus(lhs,rhs)    =>
      operation(sym)(x => append(new FloatAddNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))))
    case FloatMinus(lhs,rhs)   =>
      operation(sym)(x => append(new FloatSubNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))))
    case FloatTimes(lhs,rhs)   =>
      operation(sym)(x => append(new FloatMulNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))))
    case FloatDivide(lhs,rhs)  =>
      operation(sym)(x => append(new FloatDivNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))))

    // Intenger
    case IntPlus(lhs,rhs)      =>
      operation(sym)(x => append(new IntegerAddNode(Kind.Int, x(0), x(1))))
    case IntMinus(lhs,rhs)     =>
      operation(sym)(x => append(new IntegerSubNode(Kind.Int, x(0), x(1))))
    case IntTimes(lhs,rhs)     =>
      operation(sym)(x => append(new IntegerMulNode(Kind.Int, x(0), x(1))))
    case IntDivide(lhs,rhs)    =>
      operation(sym)(x => append(new IntegerDivNode(Kind.Int, x(0), x(1))))
    case IntMod(lhs,rhs)       =>
      operation(sym)(x => append(new IntegerRemNode(Kind.Int, x(0), x(1))))

    // Conversions
    case IntToLong(lhs) =>
      convert(sym, lhs, ConvertNode.Op.I2L)
    case IntToFloat(lhs) =>
      convert(sym, lhs, ConvertNode.Op.I2F)
    case IntToDouble(lhs) =>
      convert(sym, lhs, ConvertNode.Op.I2D)
    case FloatToInt(lhs) =>
      convert(sym, lhs, ConvertNode.Op.F2I)
    case FloatToDouble(lhs) =>
      convert(sym, lhs, ConvertNode.Op.F2D)
    case DoubleToInt(lhs) =>
      convert(sym, lhs, ConvertNode.Op.D2I)
    case DoubleToFloat(lhs) =>
      convert(sym, lhs, ConvertNode.Op.D2F)

    case _ => super.emitNode(sym, rhs)
  }

}
