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

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case PrintF(f,x) => ???//emitValDef(sym, "printf(" + (f::x.map(quote)).mkString(",") + ")")
      case PrintLn(s)  =>
        insert(sym)
        push(Const(Predef), s)
        if (s.tp.toString != "java.lang.String") {
          if (isPrimitiveType(s.tp)) {
            s.tp.toString match {
              case "Int" =>
                push(Const(Int), s)
                invoke(scala.Int.getClass, "toString", classOf[Int])
            }
          }
        }
        invoke(Predef.getClass, "println", classOf[Any])
      case Print(s)    =>
        insert(sym)
        push(Const(Predef), s)
        if (s.tp.toString != "java.lang.String") {
          if (isPrimitiveType(s.tp)) {
            s.tp.toString match {
              case "Int" =>
                push(Const(Int), s)
                invoke(scala.Int.getClass, "toString", classOf[Int])
            }
          }
        }
        invoke(Predef.getClass, "print", classOf[Any])
      case Exit(a)     => ???//emitValDef(sym, "exit(" + quote(a) + ")")
      case Return(x)   => ???//emitValDef(sym, "return " + quote(x))
      case Error(s)    => ???//emitValDef(sym, "error(" + quote(s) + ")")
      case _ => super.emitNode(sym, rhs)
    }
  }
}