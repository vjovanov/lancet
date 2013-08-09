package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenEqual extends GraalNestedCodegen with GraalBuilder with GraalGenBase {
  val IR: EqualExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Equal(a,b) => ssa(sym) {
      if(kind(a) == kind(b) && isPrimitiveType(a.tp)) {
        push(a, b)
        if_g(Condition.EQ, {
          frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
        }, {
          frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
        })
      } else {
        ??? // here we need the method invocations
      }
    }
    case NotEqual(a,b) => ssa(sym) {
      if(kind(a) == kind(b) && isPrimitiveType(a.tp)) {
        push(a, b)
        if_g(Condition.NE, {
          frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
        }, {
          frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
        })
      } else {
        ??? // here we need the method invocations
      }
    }
    case _ => super.emitNode(sym, rhs)
  }
}