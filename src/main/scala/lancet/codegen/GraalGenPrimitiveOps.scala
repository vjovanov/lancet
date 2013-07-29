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
      push(rhs)
      push(lhs)
      val addLhs = frameState.pop(Kind.Int)
      val addRhs = frameState.pop(Kind.Int)
      assert(addLhs != null && addRhs != null, "Popped state must be initialized.")
      frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, addLhs, addRhs)))
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

    case _ => super.emitNode(sym, rhs)
  }
}
