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
    // Intenger
    case NumericPlus(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" =>
            new IntegerAddNode(Kind.Int, x(0), x(1))
          case "Long" =>
            new IntegerAddNode(Kind.Long, x(0), x(1))
          case "Double" =>
            new FloatAddNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))
          case "Float" =>
            new FloatAddNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))
        }
        graph.unique(node)
      }
    case NumericTimes(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" =>
            new IntegerMulNode(Kind.Int, x(0), x(1))
          case "Long" =>
            new IntegerMulNode(Kind.Long, x(0), x(1))
          case "Double" =>
            new FloatMulNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))
          case "Float" =>
            new FloatMulNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))
        }
        graph.unique(node)
      }

    case NumericMinus(lhs,rhs)      =>
      operation(sym){x =>
        val node = tpString(sym) match {
          case "Int" =>
            new IntegerSubNode(Kind.Int, x(0), x(1))
          case "Long" =>
            new IntegerSubNode(Kind.Long, x(0), x(1))
          case "Double" =>
            new FloatSubNode(Kind.Float, x(0), x(1), isStrict(method.getModifiers()))
          case "Float" =>
            new FloatSubNode(Kind.Double, x(0), x(1), isStrict(method.getModifiers()))
        }
        graph.unique(node)
      }

    case _ => super.emitNode(sym, rhs)
  }

}
