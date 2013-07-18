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
import scala.collection.mutable.{Map => MMap, ArrayBuffer}
import scala.virtualization.lms.internal._
import lancet.core._

import java.util.concurrent.Callable
import java.util.BitSet

import com.oracle.graal.phases._   // PhasePlan
import com.oracle.graal.phases.common._
import com.oracle.graal.phases.PhasePlan.PhasePosition
import com.oracle.graal.hotspot._
import com.oracle.graal.java._
import com.oracle.graal.nodes._
import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
import com.oracle.graal.java._          // GraphBuilderConfiguration
import com.oracle.graal.graph._
import com.oracle.graal.nodes.{java=>J,_}   // StructuredGraph
import com.oracle.graal.nodes.java._        // MethodCallTargetNode
import com.oracle.graal.debug._;
import collection.JavaConversions._
import com.oracle.graal.debug.internal._
import com.oracle.graal.printer._
import com.oracle.graal.api.meta._



trait GEN_Graal_LMS extends GraalNestedCodegen with GraalCompile { self: GraalBuilder =>
  val IR: Expressions with Effects
  import IR._
  import graphBuilder._

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A], method: ResolvedJavaMethod): List[(Sym[Any], Any)] = {
    val staticData = getFreeDataBlock(body)

    // initialize the stack with function arguments
    args.foreach(insert)
    // initialize the graph builder state
    graphBuilder.init(new StructuredGraph(method))
    // Construction
    lastInstr = graph.start()
    // TODO we will need the minimum tracking of live variables in blocks
    // var removeLocals = new BitSet()
    // frameState.clearNonLiveLocals(removeLocals)
    // finish the start block
    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);

    // LMS code generation
    emitBlock(body)
    push(body.res) // push the block result for the return

    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);

    val node = frameState.pop(kind(body.res)) // TODO make an abstraction here with symbols
    frameState.clearStack();
    val retNode = new ReturnNode(node)
    graph.add(retNode)
    lastInstr.setNext(retNode)

    graphBuilder.finalize()

    staticData
  }
}

trait GraalCompile { self: GEN_Graal_LMS =>

  val compiler = HotSpotGraalRuntime.getInstance().getCompiler();
  val backend = HotSpotGraalRuntime.getInstance().getBackend();
  val target = HotSpotGraalRuntime.getInstance().getTarget();
  val cache = HotSpotGraalRuntime.getInstance().getCache();

  def compile[A, B](f: A => B): A => B = {
    // TODO reflectively create a function class with an adequate number of params (check with tiark)
    // compile the IR that you have to this function
    val cls = f.getClass
    val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
    val method = runtime.lookupJavaMethod(reflectMeth)

    val plan = new PhasePlan()
    plan.addPhase(PhasePosition.AFTER_PARSING, new GraphBuilderPhase(runtime, config, OptimisticOptimizations.ALL))
    plan.addPhase(PhasePosition.HIGH_LEVEL, printGraph("HIGH_LEVEL"))
    plan.addPhase(PhasePosition.MID_LEVEL, printGraph("MID_LEVEL"))

    val result = topScope(method) {

      // Building how the graph should look like
      val res = GraalCompiler.compileMethod(runtime, backend, target, method, graph, cache, plan, OptimisticOptimizations.ALL)
      println("Scope " + com.oracle.graal.debug.internal.DebugScope.getInstance.getQualifiedName)
      println("===== DONE")

      res
    }

    val compiledMethod = runtime.addMethod(method, result, null)

    { (x:A) =>
      val y = compiledMethod.executeVarargs(f, x.asInstanceOf[AnyRef])
      y.asInstanceOf[B]
    }
  }

  // --------- Util

  def topScope[A](method: ResolvedJavaMethod)(body: => A) = {
    //val hotspotDebugConfig = new HotSpotDebugConfig(GraalOptions.Log + ",Escape", GraalOptions.Meter, GraalOptions.Time, GraalOptions.Dump, GraalOptions.MethodFilter, System.out)
    val hotspotDebugConfig =
      new GraalDebugConfig(GraalOptions.Log,
       GraalOptions.Meter,
       GraalOptions.Time,
       GraalOptions.Dump,
       GraalOptions.MethodFilter,
       System.out,
       List(new GraphPrinterDumpHandler())
      )

    Debug.setConfig(hotspotDebugConfig)
    Debug.scope("LMS", method, new Callable[A] {
        def call: A = {
          body
        }
    });
  }

  def printGraph(s: String, verbosity: Node.Verbosity = Node.Verbosity.Short) = phase { graph =>
    println("===== " + s)
    graph.getNodes.foreach(n => println(n.toString(verbosity) + n.inputs().map(_.toString(Node.Verbosity.Id)).mkString("(",",",")")))
    println("----- " + s + " method calls ")
    graph.getNodes(classOf[InvokeNode]).foreach(printInvoke)
  }

  def printInvoke(invoke: InvokeNode): Unit = {
    val methodCallTarget = invoke.methodCallTarget()
    val targetMethod = methodCallTarget.targetMethod() // ResolvedJavaMethod

    println("  invoke: " + invoke)
    println("    trgt: " + targetMethod)
    println("    args: " + methodCallTarget.arguments())

    val assumptions = new Assumptions(true)

    val info = InliningUtil.getInlineInfo(methodCallTarget.invoke(), assumptions, OptimisticOptimizations.ALL)
    println("    info: " + info)
  }

  def phase(f: StructuredGraph => Unit) = new Phase {
    def run(graph: StructuredGraph) = f(graph)
  }

}

trait GraalGenBase extends BlockTraversal {
  val IR: Expressions
  import IR._

  val runtime = HotSpotGraalRuntime.getInstance().getRuntime();
  val config = new GraphBuilderConfiguration(GraphBuilderConfiguration.ResolvePolicy.Eager, null) // resolve eagerly, lots of DeoptNodes otherwise

  // graal stuff imported
  val graphBuilder: LancetGraphBuilder = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL)
  def graph: StructuredGraph = graphBuilder.currentGraph

  var analysisResults: MMap[String,Any] = null.asInstanceOf[MMap[String,Any]]

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

  /* until the api stabilizes
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
  }*/

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A], method: ResolvedJavaMethod): List[(Sym[Any], Any)]

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

trait GraalBuilder { self: GraalGenBase =>
  import IR._
  import graphBuilder._

  val stack: ArrayBuffer[Sym[Any]] = new ArrayBuffer[Sym[Any]]

  def kind(e: Exp[Any]) = e.tp.toString match {
    case "Boolean"=> Kind.Boolean
    case "Byte"   => Kind.Byte
    case "Char"   => Kind.Char
    case "Short"  => Kind.Short
    case "Int"    => Kind.Int
    case "Long"   => Kind.Long
    case "Float"  => Kind.Float
    case "Double" => Kind.Double
    case _ => Kind.Object
  }

  def push(c: Exp[Any]): Unit = c match {
    case Const(v: Int) =>
      frameState.ipush(ConstantNode.forConstant(Constant.forInt(v), runtime, graph))
    case Const(v: Boolean) =>
      frameState.ipush(ConstantNode.forConstant(Constant.forBoolean(v), runtime, graph))
    case sym@Sym(v) =>
      val loc = frameState.loadLocal(lookup(sym))
      frameState.push(kind(sym), loc)
  }

  def lookup(s: Sym[Any]): Int =
    stack.indexOf(s) + 1 // + 1 for this

  def insert(s: Sym[Any]): Unit =
    if(!stack.contains(s)) stack += s
}

trait GraalNestedCodegen extends GraalGenBase with NestedBlockTraversal {
  val IR: Expressions with Effects
  import IR._

  override def traverseStm(stm: Stm) = super[GraalGenBase].traverseStm(stm)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

}