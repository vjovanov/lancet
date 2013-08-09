package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenIfThenElse extends GraalNestedCodegen {
  val IR: IfThenElseExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case IfThenElse(c,thenBlock,elseBlock) => ssa(sym) {
      if_g(c, thenBlock, elseBlock)
    }
    case _ => super.emitNode(sym, rhs)
  }
}
