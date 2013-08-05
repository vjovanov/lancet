package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait ScalaGenBooleanOps extends GraalGenBase with GraalBuilder {
  val IR: BooleanOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case BooleanNegate(b)    => ???//emitValDef(sym, "!" + quote(b))
    case BooleanAnd(lhs,rhs) => ???
      // if (lhs == 1) {
      //   if(rhs == 1) {
      //     1
      //   }
      // } else 0
    case BooleanOr(lhs,rhs)  => ???
      // if (lhs == 0) {
      //   if(rhs == 0) {
      //     0
      //   }
      // } else 0
    case _ => super.emitNode(sym,rhs)
  }
}