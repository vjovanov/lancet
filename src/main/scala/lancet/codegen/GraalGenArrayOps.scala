package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes.calc._

object LancetSystem {
  def arraycopy(a: Any, b: Int, c: Any, d: Int, e: Int) =
    _root_.java.lang.System.arraycopy(a,b,c,d,e)
}

trait GraalGenArrayOps extends GraalNestedCodegen with GraalBuilder {
  val IR: ArrayOpsExp
  import IR._
  import graphBuilder._
  val ARRAY_LITERAL_MAX_SIZE = 1000

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@ArrayNew(n) =>
      insert(sym)
      push(n)
      val code = sym.tp.typeArguments.head.toString match {
        case "Long" => 11
        case "Int" => 10
        case "Short" => 9
        case "Byte" => 8
        case "Double" => 7
        case "Float" => 6
        case "Char" => 5
        case "Boolean" => 4
      }
      genNewPrimitiveArray(code)
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
      insert(sym)
      push(Const(LancetSystem))
      push(src,srcPos,dest,destPos,len)
      invoke(LancetSystem.getClass, "arraycopy", classOf[Any], classOf[Int], classOf[Any], classOf[Int], classOf[Int] )
    case a@ArraySort(x) =>
    case n@ArrayMap(a,x,blk) =>
    case ArrayToSeq(a) =>
    case e@ArrayFromSeq(xs) =>
    case _ => super.emitNode(sym, rhs)
  }
}
