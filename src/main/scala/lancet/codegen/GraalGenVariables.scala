package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenVariables extends GraalNestedCodegen with GraalBuilder {
  val IR: VariablesExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewVar(c) => ssa(sym) {
      push(c)
    }
    case ReadVar(Variable(a)) =>
      insert(sym, a.asInstanceOf[Sym[Any]]) // alias stack position
      push(a)
      storeLocal(kind(a), lookup(sym))
    case VarPlusEquals(Variable(a), b) =>
      ??? // Maybe fix these malitious cases with lowering. Check with Tiark.
    case Assign(Variable(a), b) =>
      insert(sym, a.asInstanceOf[Sym[_]]) // alias stack position
      push(b)
      storeLocal(kind(a), lookup(a.asInstanceOf[Sym[Any]]))
    case _ => super.emitNode(sym, rhs)
  }
}