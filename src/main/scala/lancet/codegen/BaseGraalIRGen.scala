/*
 * Copyright (c) 2009, 2012, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package lancet.codegen

import scala.virtualization.lms.util.GraphUtil
import java.io.{File, PrintWriter}
import scala.reflect.RefinedManifest
import scala.collection.mutable.{Map => MMap}
import scala.virtualization.lms.internal._
import lancet.core._

import com.oracle.graal.phases._   // PhasePlan
import com.oracle.graal.phases.common._
import com.oracle.graal.hotspot._
import com.oracle.graal.java._
import com.oracle.graal.nodes._

trait GEN_Graal_LMS extends BaseGraalIRGen with GraalCompile {
  val IR: Expressions with Effects
  import IR._

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A]): List[(Sym[Any], Any)] = {
    val staticData = getFreeDataBlock(body)

    emitBlock(body)

    staticData
  }

}

trait GraalCompile {
  def compile[A, B](): A => B = {
    // reflectively create a function class with an adequate number of params

    // compile the IR that you have to this function
  }
}

trait BaseGraalIRGen extends NestedBlockTraversal {
  val IR: Expressions with Effects
  import IR._

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  val graphBuilder: LancetGraphBuilder = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL)

  // graal stuff imported
  var analysisResults: MMap[String,Any] = null.asInstanceOf[MMap[String,Any]]

  def currentGraph: StructuredGraph = graphBuilder.currentGraph
  // Initializer
  def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = { analysisResults = _analysisResults }
  def finalizeGenerator(): Unit = {}

  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  // TODO do we need this?
  def remap(s: String): String = s
  def remap[A](s: String, method: String, t: Manifest[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"
  def remap[A](m: Manifest[A]): String = m match {
    case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
    case _ if m.erasure == classOf[Variable[Any]] =>
        remap(m.typeArguments.head)
    case _ =>
      // call remap on all type arguments
      val targs = m.typeArguments
      if (targs.length > 0) {
        val ms = m.toString
        ms.take(ms.indexOf("[")+1) + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else m.toString
  }
  def remapImpl[A](m: Manifest[A]): String = remap(m)
  //def remapVar[A](m: Manifest[Variable[A]]) : String = remap(m.typeArguments.head)

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  // ----------

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def emit[T : Manifest, R : Manifest](f: Exp[T] => Exp[R]): List[(Sym[Any], Any)] = {
    val s = fresh[T]
    val body = reifyBlock(f(s))
    emit(List(s), body)
  }

  def emit2[T1 : Manifest, T2 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2]) => Exp[R]): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val body = reifyBlock(f(s1, s2))
    emit(List(s1, s2), body)
  }

  def emit3[T1 : Manifest, T2 : Manifest, T3 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R]): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val body = reifyBlock(f(s1, s2, s3))
    emit(List(s1, s2, s3), body)
  }

  def emit4[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R]): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    emit(List(s1, s2, s3, s4), body)
  }

  def emit5[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R]): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val body = reifyBlock(f(s1, s2, s3, s4, s5))
    emit(List(s1, s2, s3, s4, s5), body)
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A]): List[(Sym[Any], Any)] // return free static data in block

  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => ???
    case Const(c: Char) => ???
    case Const(f: Float) => ???
    case Const(l: Long) => ???
    case Const(null) => ???
    case Const(z) => ???
    case Sym(n) => ???
    case _ => throw new RuntimeException("could not quote " + x)
  }

  // ----------

  override def reset {
    super.reset
  }


  def isPrimitiveType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" => true
      case _ => false
    }
  }

  def isVoidType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Unit" => true
      case _ => false
    }
  }

  def isVariableType[A](m: Manifest[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }

}



trait GenericNestedCodegen extends NestedBlockTraversal with BaseGraalIRGen {
  val IR: Expressions with Effects
  import IR._

  override def traverseStm(stm: Stm) = super[BaseGraalIRGen].traverseStm(stm)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

}