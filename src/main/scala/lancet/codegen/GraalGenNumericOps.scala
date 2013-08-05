package lancet.codegen

import java.lang.reflect.Modifier._

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenNumbericOps extends GraalGenBase with GraalBuilder {
  val IR: NumericOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case NumericPlus(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" | "Long" =>
            new IntegerAddNode(kind(sym), x(0), x(1))
          case "Double" | "Float" =>
            new FloatAddNode(kind(sym), x(0), x(1), isStrict(method.getModifiers()))
        }
        append(node)
      }

    case NumericTimes(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" | "Long" =>
            new IntegerMulNode(kind(sym), x(0), x(1))
          case "Double" | "Float" =>
            new FloatMulNode(kind(sym), x(0), x(1), isStrict(method.getModifiers()))
        }
        append(node)
      }

    case NumericMinus(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" | "Long" =>
            new IntegerSubNode(kind(sym), x(0), x(1))
          case "Double" | "Float" =>
            new FloatSubNode(kind(sym), x(0), x(1), isStrict(method.getModifiers()))
        }
        append(node)
      }

    case NumericDivide(lhs,rhs)      =>
      operation(sym){x =>
        tpString(sym) match {
          case "Int" | "Long" =>
            append(new IntegerDivNode(kind(sym), x(0), x(1)))
          case "Double" | "Float" =>
            append(new FloatDivNode(kind(sym), x(0), x(1), isStrict(method.getModifiers())))
        }
      }


    case _ => super.emitNode(sym, rhs)
  }

}
