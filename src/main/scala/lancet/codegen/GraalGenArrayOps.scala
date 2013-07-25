package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenArrayOps extends GraalNestedCodegen with GraalBuilder {
  val IR: ArrayOpsExp
  import IR._
  import graphBuilder._
  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) =>
    case e@ArrayFromSeq(xs) =>
    case ArrayApply(x,n) =>
    case ArrayUpdate(x,n,y) =>
    case ArrayLength(x) =>
    case ArrayForeach(a,x,block) =>
    case ArrayCopy(src,srcPos,dest,destPos,len) =>
    case a@ArraySort(x) =>
    case n@ArrayMap(a,x,blk) =>
    case ArrayToSeq(a) =>
    case _ => super.emitNode(sym, rhs)
  }
}
