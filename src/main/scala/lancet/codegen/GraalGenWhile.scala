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
         loopBegin.setStateAfter(frameState.create(0))

         // We have seen all forward branches. All subsequent backward branches will merge to the
         // loop header.
         // This ensures that the loop header has exactly one non-loop predecessor.
         val loopFristInstr = loopBegin;
         // We need to preserve the frame state builder of the loop header so that we can merge
         // values for
         // phi functions, so make a copy of it.
         val loopBlockState = frameState.copy() // why is this not used
         frameState = loopBlockState
         // emit the conditional
         emitBlock(c)
         push(c.res)
         // push(Const(1))
         val (thn, els) = ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), true, (loopFristInstr, loopBlockState));
         val frameStateThen = frameState//.copy() // when should you do a state copy?
         val frameStateElse = frameState.copy() //this is a loop exit

         // starting the then block
         val entryState = frameState
         frameState = frameStateElse //loopBlockState
         lastInstr = els

         // emit loop block
         emitBlock(b)

         // jump to the loop beginning
         appendGoto({
           val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState);
           val result1 = target.fixed;
           entryState.merge(loopBegin, target.state);
           result1
         })
         // else block (after loop)
         lastInstr = thn
         frameState = frameStateThen
    case _ => super.emitNode(sym, rhs)
  }
}