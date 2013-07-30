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
      insert(sym)
      push(n)
      genNewPrimitiveArray(4) // TODO make a manifest based case
      storeLocal(Kind.Object, lookup(sym))
    case ArrayApply(x,n) =>
      insert(sym)
      push(x)
      push(n)
      genLoadIndexed(kind(sym))
      storeLocal(kind(sym), lookup(sym))
    case ArrayUpdate(x,n,y) =>
      push(x)
      push(n)
      push(y)
      genStoreIndexed(kind(y))
    case ArrayLength(x) =>
      insert(sym)
      push(x)
      genArrayLength()
      storeLocal(Kind.Int, lookup(sym))
    case ArrayForeach(a,x,block) =>
    case ArrayCopy(src,srcPos,dest,destPos,len) =>
    case a@ArraySort(x) =>
    case n@ArrayMap(a,x,blk) =>
    case ArrayToSeq(a) =>
    case e@ArrayFromSeq(xs) =>
    case _ => super.emitNode(sym, rhs)
  }
}
