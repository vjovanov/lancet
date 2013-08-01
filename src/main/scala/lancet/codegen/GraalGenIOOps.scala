package lancet.codegen

import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack
import scala.reflect.SourceContext
import java.io.PrintWriter
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes._
import com.oracle.graal.nodes.calc._

trait GraalGenIOOps extends GraalNestedCodegen with GraalBuilder {
  val IR: IOOpsExp
  import IR._
  import graphBuilder._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ObjFileApply(dir)       => ???//emitValDef(sym, "new java.io.File(" + quote(dir) + ")")
      case FileGetCanonicalFile(f) => ???//emitValDef(sym, quote(f) + ".getCanonicalFile()")
      case FileGetPath(f)          => ???//emitValDef(sym, quote(f) + ".getPath()")
      case FileListFiles(f)        => ???//emitValDef(sym, quote(f) + ".listFiles()")
      case ObjBrApply(f)           => ???//emitValDef(sym, "new java.io.BufferedReader(" + quote(f) + ")")
      case ObjBwApply(f)           => ???//emitValDef(sym, "new java.io.BufferedWriter(" + quote(f) + ")")
      case ObjFrApply(s)           => ???//emitValDef(sym, "new java.io.FileReader(" + quote(s) + ")")
      case ObjFwApply(s)           => ???//emitValDef(sym, "new java.io.FileWriter(" + quote(s) + ")")
      case BwWrite(b,s)            => ???//emitValDef(sym, quote(b) + ".write(" + quote(s) + ")")
      case BwClose(b)              => ???//emitValDef(sym, quote(b) + ".close()")
      case BrReadline(b)           => ???//emitValDef(sym, quote(b) + ".readLine()")
      case BrClose(b)              => ???//emitValDef(sym, quote(b) + ".close()")
      case _                       => super.emitNode(sym, rhs)
    }
  }
}