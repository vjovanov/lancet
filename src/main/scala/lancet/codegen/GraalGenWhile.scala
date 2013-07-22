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

    case _ => super.emitNode(sym, rhs)
  }
}