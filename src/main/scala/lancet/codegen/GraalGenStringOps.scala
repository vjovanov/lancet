package lancet.codegen

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenStringOps extends GraalNestedCodegen with GraalBuilder {
  val IR: StringOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  {
      rhs match {
        case StringPlus(s1,s2)       =>
          insert(sym)
          s2.tp.toString match {
            case "java.lang.String" =>
              push(s1, s2)
              invoke(classOf[String], "concat", classOf[String])
              storeLocal(kind(sym), lookup(sym))
            case "Int" =>
              push(s1)
              // push(Const(Conversions), s2)
              // invoke(lancet.codegen.Conversions.getClass, "i2s", classOf[Int])
              push(Const("conversion"))
              invoke(classOf[String], "concat", classOf[String])
              Predef.println("Store = " + frameState)
              storeLocal(kind(sym), lookup(sym))
          }
        case StringStartsWith(s1,s2) => ???
        case StringTrim(s)           => ???
        case StringSplit(s, sep)     => ???
        case StringEndsWith(s, e)    => ???
        case StringCharAt(s,i)       => ???
        case StringValueOf(a)        => ???
        case StringToDouble(s)       => ???
        case StringToFloat(s)        => ???
        case StringToInt(s)          => ???
        case StringContains(s1,s2)   => ???
        case _ => super.emitNode(sym, rhs)
    }
  }
}