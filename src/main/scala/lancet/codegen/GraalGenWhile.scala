package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenWhile extends GraalNestedCodegen with GraalBuilder {
  val IR: WhileExp
  import IR._
  import graphBuilder._
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case While(c,b) =>
         insert(sym)
         /* TODO this should be here only if it is the first block
         val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode());
         val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState);
         val result = target.fixed;
         val tmpState = frameState.copy()
         appendGoto(result)
         frameState = tmpState
         lastInstr = nextFirstInstruction*/

         // Loop
         // starting the loop block
         val preLoopEnd = currentGraph.add(new EndNode())
         val loopBegin = currentGraph.add(new LoopBeginNode())
         lastInstr.setNext(preLoopEnd)
         // Add the single non-loop predecessor of the loop header.
         loopBegin.addForwardEnd(preLoopEnd)
         lastInstr = loopBegin

         // Create phi functions for all local variables and operand stack slots.
         frameState.insertLoopPhis(loopBegin)
         loopBegin.setStateAfter(frameState.create(4))

         val loopFristInstr = loopBegin
         val loopBlockState = frameState.copy()

         frameState = loopBlockState
         lastInstr = loopBegin
         // clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] load the condition variables
         emitBlock(c)
         push(c.res)
         // [end]
         // clearLocals(liveOut)

         val ((thn, frameStateThen), (els, frameStateElse)) =
           ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true, (loopBegin, loopBlockState));

         // starting the body (else block)
         frameState = frameStateElse // should the loop block state go here?
         lastInstr = els
         // clearLocals(frameState)(1, 2, 3)
         frameState.cleanupDeletedPhis();

         // [begin] body
         emitBlock(b)
         // [end] body

         appendGoto({
           val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState)
           val result = target.fixed
           loopBlockState.merge(loopBegin, target.state)
           result
         })

         // after loop (then block)
         frameState = frameStateThen
         lastInstr = thn
    case _ => super.emitNode(sym, rhs)
  }
}
