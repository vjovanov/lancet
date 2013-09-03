package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes.calc._

trait GraalGenArrayOps extends GraalNestedCodegen {
  val IR: ArrayOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) => ssa(sym) {
      push(n)
      val code = sym.tp.typeArguments.head.toString match {
        case "Long"    => 11
        case "Int"     => 10
        case "Short"   => 9
        case "Byte"    => 8
        case "Double"  => 7
        case "Float"   => 6
        case "Char"    => 5
        case "Boolean" => 4
      }
      genNewPrimitiveArray(code)
    }
    case ArrayApply(x,n) => ssa(sym) {
      push(x,n)
      genLoadIndexed(kind(sym))
    }
    case ArrayUpdate(x,n,y) =>
      insert(sym)
      push(x,n,y)
      genStoreIndexed(kind(y))
    case ArrayLength(x) => ssa(sym) {
      push(x)
      genArrayLength()
    }
    case ArrayForeach(a,x,block) =>
    case ArrayCopy(src,srcPos,dest,destPos,len) =>
      insert(sym)
      push(src,srcPos,dest,destPos,len)
      invokeStatic(classOf[_root_.java.lang.System], "arraycopy", classOf[Unit], classOf[Any], classOf[Int], classOf[Any], classOf[Int], classOf[Int] )
    case a@ArraySort(x) =>
    case n@ArrayMap(a,x,blk) =>
    case ArrayToSeq(a) =>
    case e@ArrayFromSeq(xs) =>
    case _ => super.emitNode(sym, rhs)
  }
}
