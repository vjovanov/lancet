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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b)    =>

      // TODO make an abstraction for the if construct
      insert(sym)
      push(a)
      push(b)
      val lhs = frameState.pop(kind(a))
      val rhs = frameState.pop(kind(b))
      assert(rhs != null)
      assert(lhs != null)
      val (thn, els) = ifNode(lhs, Condition.LT, rhs, true, null)
      val frameStateThen = frameState.copy()
      val frameStateElse = frameState.copy()
      // then
      // here we should have a new lastInstr, and the new frameState
      lastInstr = thn
      frameState = frameStateThen

      // TODO clearLocals(frameState)
      frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
      storeLocal(kind(sym), lookup(sym))
      // appendGoto(createTarget(probability, currentBlock.successors.get(0), frameState));
      var exitState = frameState.copy()
      val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
      appendGoto({ // inlined create target
       val result = new LancetGraphBuilder.Target(target, frameState);
       result.fixed
      })

         // else
      lastInstr = els
      frameState = frameStateElse
      // TODO clearLocals(frameState)

      frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
      storeLocal(kind(sym), lookup(sym))

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

    case OrderingLTEQ(a,b)  =>
      ???
    case OrderingGT(a,b)    =>
      insert(sym)
      push(a)
      push(b)
      val (thn, els) = ifNode(frameState.pop(kind(a)), Condition.GT, frameState.pop(kind(b)), true, null)
      val frameStateThen = frameState.copy()
      val frameStateElse = frameState.copy()
      // then
      // here we should have a new lastInstr, and the new frameState
      lastInstr = thn
      frameState = frameStateThen

      // TODO clearLocals(frameState)
      frameState.ipush(ConstantNode.forConstant(Constant.INT_1, runtime, graph))
      storeLocal(kind(sym), lookup(sym))

      // appendGoto(createTarget(probability, currentBlock.successors.get(0), frameState));
      var exitState = frameState.copy()
      val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
      appendGoto({ // inlined create target
       val result = new LancetGraphBuilder.Target(target, frameState);
       result.fixed
      })

         // else
      lastInstr = els
      frameState = frameStateElse
      // TODO clearLocals(frameState)

      frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
      storeLocal(kind(sym), lookup(sym))

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
    case OrderingGTEQ(a,b)  =>
      ???
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