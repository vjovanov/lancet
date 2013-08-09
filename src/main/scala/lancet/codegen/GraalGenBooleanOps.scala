package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait ScalaGenBooleanOps extends GraalCodegen {
  val IR: BooleanOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b)    => ???
    case BooleanAnd(lhs,rhs) => ssa(sym) {
      push(lhs, Const(1))
      if_g(Condition.EQ, {
          push(rhs, Const(1))
          if_g(Condition.EQ, {
            push(Const(1))
          }, {
            push(Const(0))
          })
      }, {
          push(Const(0))
      })
    }
    case BooleanOr(lhs,rhs)  => ssa(sym) {
      push(lhs, Const(0))
      if_g(Condition.EQ, {
        push(rhs, Const(0))
        if_g(Condition.EQ, {
          push(Const(0))
        }, {
          push(Const(1))
        })
      }, {
        push(Const(1))
      })
    }
    case _ => super.emitNode(sym,rhs)
  }
}