package lancet.codegen

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._


trait GraalGenOrderingOps extends GraalNestedCodegen with GraalBuilder {
  val IR: OrderingOpsExp
  import IR._
  import graphBuilder._

  def condition(c: Condition)(sym: Sym[_], a: Exp[_], b: Exp[_]): Unit = {
    assert(kind(a) == kind(b), "Can not compare different primitive types!!!")
    insert(sym)
    val (lhs, cond, rhs) = kind(a) match {
      case Kind.Int  =>
        push(a, b)
        val rhs = frameState.pop(kind(b))
        val lhs = frameState.pop(kind(a))
        (lhs, c.negate, rhs)
      case Kind.Long =>
        push(a,b) // gen compare
        c match {
          case Condition.GE | Condition.GT | Condition.EQ | Condition.NE =>
            genCompareOp(kind(a), true)
          case Condition.LE | Condition.LT =>
            genCompareOp(kind(a), false)
        }
        val rhs = appendConstant(Constant.INT_0)
        (frameState.ipop(), c.negate, rhs)
      case Kind.Double | Kind.Float =>
        push(a,b) // gen coempare
        c match {
          case Condition.GE | Condition.GT | Condition.EQ | Condition.NE =>
            genCompareOp(kind(a), true)
          case Condition.LE | Condition.LT =>
            genCompareOp(kind(a), false)
        }
        val rhs = appendConstant(Constant.INT_0)
        (frameState.ipop(), c.negate, rhs)
    }


    val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(lhs, cond, rhs, true, null)
    // else
    lastInstr = els
    frameState = frameStateElse

    frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
    var exitState = frameState.copy()
    val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
    appendGoto({ // inlined create target
     val result = new LancetGraphBuilder.Target(target, frameState);
     result.fixed
    })

    // then
    lastInstr = thn
    frameState = frameStateThen
    frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))

    // The EndNode for the already existing edge.
    val end = currentGraph.add(new EndNode());
    // The MergeNode that replaces the placeholder.
    val mergeNode = currentGraph.add(new MergeNode());
    appendGoto({ // inlined create target
      val next = target.next();

      target.setNext(end);
      mergeNode.addForwardEnd(end);
      mergeNode.setNext(next);

      // The EndNode for the newly merged edge.
      val newEnd = currentGraph.add(new EndNode())
      val target2 = new LancetGraphBuilder.Target(newEnd, frameState);
      val result = target2.fixed;
      exitState.merge(mergeNode, target2.state);
      mergeNode.addForwardEnd(newEnd);
      result
    })
    frameState = exitState
    lastInstr = mergeNode
    mergeNode.setStateAfter(frameState.create(0))
    storeLocal(kind(sym), lookup(sym))
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b)    =>
      condition(Condition.LT)(sym, a, b)
    case OrderingGT(a,b)    =>
      condition(Condition.GT)(sym, a, b)
    case OrderingGTEQ(a,b)  =>
      condition(Condition.GE)(sym, a, b)
    case OrderingLTEQ(a,b)  =>
      condition(Condition.LE)(sym, a, b)
    case OrderingEquiv(a,b) =>
      ???
    case OrderingMax(a,b)   =>
      ???
    case OrderingMin(a,b)   =>
      ???

    case _                  =>
      super.emitNode(sym, rhs)
  }
}
