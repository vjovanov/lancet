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

  def condition(c: Condition)(sym: Sym[_], a: Exp[_], b: Exp[_]): Unit = ssa(sym) {
    assert(kind(a) == kind(b), "Can not compare different primitive types!!!")
    kind(a) match {
      case Kind.Int  =>
        push(a, b)
      case Kind.Long | Kind.Double | Kind.Float =>
        push(a,b) // gen compare
        c match {
          case Condition.GE | Condition.GT | Condition.EQ | Condition.NE =>
            genCompareOp(kind(a), true)
          case Condition.LE | Condition.LT =>
            genCompareOp(kind(a), false)
        }
        push(Const(0))
    }

    if_g(c.negate, {push(Const(0))}, {push(Const(1))})
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case OrderingLT(a,b)    =>
      condition(Condition.LT)(sym, a, b)
    case OrderingGT(a,b)    =>
      condition(Condition.GT)(sym, a, b)
    case OrderingGTEQ(a,b)  =>
      condition(Condition.GE)(sym, a, b)
    case OrderingLTEQ(a,b)  =>
      condition(Condition.LE)(sym, a, b)
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
