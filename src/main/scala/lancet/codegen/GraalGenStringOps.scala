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
          pushToString(s1, s2)
          invoke(classOf[String], "concat", classOf[String], classOf[String])
        }
        case StringStartsWith(s1,s2) => ssa(sym) {
          push(s1, s2)
          invoke(classOf[String], "startsWith", classOf[Boolean], classOf[String])
        }
        case StringContains(s1,s2)   => ssa(sym) {
          push(s1, s2)
          invoke(classOf[String], "endsWith", classOf[Boolean], classOf[String])
        }
        case StringSplit(s, sep)     => ssa(sym) {
          push(s, sep)
          invoke(classOf[String], "split", classOf[String], classOf[String])
        }
        case StringEndsWith(s, e)    => ssa(sym) {
          push(s, e)
          invoke(classOf[String], "endsWith", classOf[Boolean], classOf[String])
        }
        case StringCharAt(s,i)       => ssa(sym) {
          push(s, i)
          invoke(classOf[String], "charAt", classOf[Char], classOf[Int])
        }
        case StringTrim(s)           => ssa(sym) {
          push(s)
          invoke(classOf[String], "trim", classOf[String])
        }
        case StringValueOf(a)        => ssa(sym) {
          ???
        }
        case StringToDouble(s)       => ssa(sym) {
          push(Const(Predef), s)
          invoke(Predef.getClass, "augmentString", classOf[String], classOf[String])
          invoke(classOf[scala.collection.immutable.StringOps], "toDouble", classOf[Double])
        }
        case StringToFloat(s)        => ssa(sym) {
          push(Const(Predef), s)
          invoke(Predef.getClass, "augmentString", classOf[String], classOf[String])
          invoke(classOf[scala.collection.immutable.StringOps], "toFloat", classOf[Float])
        }
        case StringToInt(s)          => ssa(sym) {
          push(Const(Predef), s)
          invoke(Predef.getClass, "augmentString", classOf[String], classOf[String])
          invoke(classOf[scala.collection.immutable.StringOps], "toInt", classOf[Int])
        }
        case _ => super.emitNode(sym, rhs)
    }
  }
}