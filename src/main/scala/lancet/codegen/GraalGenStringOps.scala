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
        case StringPlus(s1,s2)       => ssa(sym) {
          tpString(s2) match {
            case "java.lang.String" =>
              push(s1, s2)
              invoke(classOf[String], "concat", classOf[String])
            case _ =>
              push(s1)
              pushToString(s2)
              invoke(classOf[String], "concat", classOf[String])
          }
        }
        case StringStartsWith(s1,s2) => ssa(sym) {
          push(s1, s2)
          invoke(classOf[String], "startsWith", classOf[String])
        }
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