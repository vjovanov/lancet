package lancet.codegen

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenMiscOps extends GraalNestedCodegen with GraalBuilder {
  val IR: MiscOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Return(x)   => throw GraalBackendException("Not supported by the Graal back-end")
    case PrintF(f,x) => ssa(sym) {
      push(Const(Predef))
      push(Const(f))
      x foreach { x => pushToString(x) }
      invoke(Predef.getClass, "printf", (classOf[String] +: (0 until (x.length)) map (x => classOf[Any])):_*)
    }
    case PrintLn(s)  => ssa(sym) {
      push(Const(Predef))
      pushToString(s)
      invoke(Predef.getClass, "println", classOf[Any])
    }
    case Print(s)    => ssa(sym) {
      insert(sym)
      push(Const(Predef))
      pushToString(s)
      invoke(Predef.getClass, "print", classOf[Any])
    }
    case Exit(a)     => ssa(sym) {
      push(Const(Predef), a)
      invoke(Predef.getClass, "exit", classOf[Int])
    }
    case Error(s)    => ssa(sym) {
      push(Const(Predef))
      pushToString(s)
      invoke(Predef.getClass, "error", classOf[String])
    }
    case _ => super.emitNode(sym, rhs)
  }

}