package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenIfThenElse extends GraalNestedCodegen with GraalBuilder {
  val IR: IfThenElseExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case IfThenElse(c,thenBlock,elseBlock) =>
      insert(sym)
      push(c)
      val rhs = appendConstant(Constant.INT_0)
      val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(frameState.pop(Kind.Int), Condition.EQ, rhs, true, null)
      lastInstr = thn
      frameState = frameStateThen
      // [else] (NOTE: That is how scala and java do it. Yes else goes into the then block.)

      emitBlock(elseBlock)
      push(elseBlock.res) // for the return value
      storeLocal(kind(sym), lookup(sym))

      // appendGoto(createTarget(probability, currentBlock.successors.get(0), frameState));
      var exitState = frameState.copy()
      val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
      appendGoto({ // inlined create target
       val result = new LancetGraphBuilder.Target(target, frameState);
       result.fixed
      })

      lastInstr = els
      frameState = frameStateElse
      // [then] (NOTE: That is how scala and java do it. Yes else goes into the then block.)

      emitBlock(thenBlock)
      push(thenBlock.res)
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
    case _ => super.emitNode(sym, rhs)
  }
}
