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
         loopBegin.setStateAfter(frameState.create(0))

         // We have seen all forward branches. All subsequent backward branches will merge to the
         // loop header.
         // This ensures that the loop header has exactly one non-loop predecessor.
         val loopFristInstr = loopBegin;
         // We need to preserve the frame state builder of the loop header so that we can merge
         // values for
         // phi functions, so make a copy of it.
         val loopBlockState = frameState.copy() // why is this not used

         // emit the conditional
         emitBlock(c)
         push(c.res)

         val (thn, els) = ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true);
         val frameStateThen = frameState.copy()
         val frameStateElse = frameState.copy()

         // starting the then block
         val entryState = frameState
         frameState = loopBlockState // should the loop block state go here?
         lastInstr = thn

         // emit loop block
         emitBlock(b)

         // 17:  goto  4
         appendGoto({
           val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState);
           val result1 = target.fixed;
           entryState.merge(loopBegin, target.state);
           result1
         })
         // else block (after loop)
         lastInstr = els
         frameState = frameStateElse
    case _ => super.emitNode(sym, rhs)
  }
}