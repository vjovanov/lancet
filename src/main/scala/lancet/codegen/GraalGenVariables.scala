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
    case NewVar(c) =>
      insert(sym)
      push(c)
      storeLocal(kind(sym), lookup(sym))
    case ReadVar(Variable(a)) =>
      insert(sym, lookup(a.asInstanceOf[Sym[Any]])) // alias stack position
      push(a)
      storeLocal(kind(a), lookup(sym))
    case VarPlusEquals(Variable(a), b) =>
      push(a)
      push(b)
      // TODO polymorphic addition. Maybe even a method call???
      frameState.push(Kind.Int, graph.unique(new IntegerAddNode(Kind.Int, frameState.pop(Kind.Int), frameState.pop(Kind.Int))))
      storeLocal(kind(a), lookup(a.asInstanceOf[Sym[Any]]))
    case Assign(Variable(a), b) =>
      push(b)
      storeLocal(kind(a), lookup(a.asInstanceOf[Sym[Any]]))

    case _ => super.emitNode(sym, rhs)
  }
}