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
    case IfThenElse(c,a,b) =>
      frameState.ipush(ConstantNode.forConstant(Constant.INT_0, runtime, graph))
      insert(sym)
      push(c)
      
      val (thn, els) = ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true)
      val frameStateThen = frameState.copy()
      val frameStateElse = frameState.copy()
      // then
      // here we have a new lastInstr, and the new frameState
      lastInstr = thn
      frameState = frameStateThen

      emitBlock(a)
      push(a.res) // for the return value
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

      emitBlock(b)
      push(b.res) // for the return value
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
