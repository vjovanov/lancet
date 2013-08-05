package lancet.codegen

import java.lang.reflect.Modifier._

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenImplicitOps extends GraalGenBase with GraalBuilder {
  val IR: ImplicitOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ImplicitConvert(x) =>
      insert(sym)
      // do a primitive type conversion
      (tpString(x), tpString(sym)) match {
        case ("Int", "Double") =>
          convert(sym, x, ConvertNode.Op.I2D)
        case ("Int", "Float") =>
          convert(sym, x, ConvertNode.Op.I2F)
        case ("Float", "Double") =>
          convert(sym, x, ConvertNode.Op.F2D)
      }
    case _ => super.emitNode(sym, rhs)
  }
}
