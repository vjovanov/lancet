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
import scala.virtualization.lms.common._
import java.io.{File, PrintWriter}
import scala.reflect.RefinedManifest
import scala.collection.mutable.{HashMap => MHashMap, Map => MMap, ArrayBuffer}
import scala.virtualization.lms.internal._
import lancet.core._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader
import java.util.concurrent.Callable
import java.util.BitSet
import java.lang.reflect.Method
import java.util.concurrent.atomic.AtomicLong
import org.objectweb.asm.Opcodes

import com.oracle.graal.java._
import com.oracle.graal.phases._   // PhasePlan
import com.oracle.graal.phases.common._
import com.oracle.graal.phases.tiers._
import com.oracle.graal.phases.PhasePlan.PhasePosition
import com.oracle.graal.hotspot._
import com.oracle.graal.nodes._
import com.oracle.graal.{java=>J,_}
import com.oracle.graal.debug._         // Debug
import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.api.code._      // Assumptions
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import com.oracle.graal.compiler._      // GraalCompiler
import com.oracle.graal.compiler.phases._      // GraalCompiler
import com.oracle.graal.graph._
import com.oracle.graal.nodes.{java=>J,_}   // StructuredGraph
import com.oracle.graal.nodes.java._        // MethodCallTargetNode
import com.oracle.graal.debug._;
import collection.JavaConversions._
import com.oracle.graal.debug.internal._
import com.oracle.graal.printer._
import com.oracle.graal.api.meta._
import com.oracle.graal.nodes.calc._
import com.oracle.graal.api.runtime._
import com.oracle.graal.nodes.spi._
import scala.collection.mutable.ArrayBuffer

object GEN_Graal_LMS {
  val classCounter = new AtomicLong(0L)
}

trait GEN_Graal_LMS extends GraalNestedCodegen with GraalCompile with ExportGraph { self: GraalBuilder =>
  val IR: Expressions with Effects
  import IR._
  import graphBuilder._

  var debugDepGraph = true
  var cls: Class[_] = _
  var method: ResolvedJavaMethod = _

  // TODO (until we are sure it works): instead of doing a lowering phase to method calls we do two passes.
  var currentCalls: ArrayBuffer[JVMMethodCall] = new ArrayBuffer()
  var methodCalls: ArrayBuffer[JVMMethodCall] = _
  override def resetGenerator(): Unit = {
    super.resetGenerator()
    cls = null
    method = null
    currentCalls = new ArrayBuffer[JVMMethodCall]()
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A], className: String = genClassName()): List[(Sym[Any], Any)] = {
    cls = FunctionTemplate.generateAndLoadFunction(className, args.map(_.tp), manifest[A], Nil)
    val reflectMeth = cls.getDeclaredMethod("apply", args.map(_.tp.runtimeClass):_*)
    method = runtime.lookupJavaMethod(reflectMeth)

    if (debugDepGraph) {
      println("dumping graph to: "+this.getClass.getName)
      exportGraph(this.getClass.getName + "-lms")(body.res)
    }


    val staticData = getFreeDataBlock(body)

    { // TODO: first phase
      // initialize the stack with function arguments
      args.foreach(insert)

      // initialize the graph builder state
      graphBuilder.init(new StructuredGraph(method))
      lastInstr = graph.start()
      // finish the start block
      lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

      frameState.cleanupDeletedPhis()
      frameState.setRethrowException(false)

      // LMS code generation
      emitBlock(body)
      push(body.res) // push the block result for the return
    }

    methodCalls = currentCalls
    resetGenerator()
    println(s"mcls = $methodCalls")

    { // phase with method call info
      cls = FunctionTemplate.generateAndLoadFunction(className + "-new", args.map(_.tp), manifest[A], methodCalls.toList)
      val reflectMeth = cls.getDeclaredMethod("apply", args.map(_.tp.runtimeClass):_*)
      method = runtime.lookupJavaMethod(reflectMeth)
      // initialize the stack with function arguments
      args.foreach(insert)

      // initialize the graph builder state
      graphBuilder.init(new StructuredGraph(method))
      lastInstr = graph.start()
      // finish the start block
      lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0));

      frameState.cleanupDeletedPhis()
      frameState.setRethrowException(false)

      // LMS code generation
      emitBlock(body)
      push(body.res) // push the block result for the return
    }

    frameState.cleanupDeletedPhis()
    frameState.setRethrowException(false)

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

  val compiler = HotSpotGraalRuntime.graalRuntime().getCompilerToVM();
  val backend = HotSpotGraalRuntime.graalRuntime().getBackend();
  val target = HotSpotGraalRuntime.graalRuntime().getTarget();
  val cache = HotSpotGraalRuntime.graalRuntime().getCache();

  def compile0(c: Class[_]): InstalledCode = {
    val plan = new PhasePlan()
    plan.addPhase(PhasePosition.AFTER_PARSING, new GraphBuilderPhase(runtime, config, OptimisticOptimizations.NONE))
    plan.addPhase(PhasePosition.AFTER_PARSING, printGraphPhase("AFTER_PARSING"))

    val result = topScope(method) {
      println("To debug use:")
      println("Scope " + com.oracle.graal.debug.internal.DebugScope.getInstance.getQualifiedName)
      println("Method " + method)

      Debug.dump(graph, "Constructed")
      printGraph("---- Before Compilation ----")
      HighTier.Inline.setValue(false)
      val start = System.currentTimeMillis()
      var res = GraalCompiler.compileGraph(
        graph,
        CodeUtil.getCallingConvention(runtime, CallingConvention.Type.JavaCallee, method, false),
        method,
        runtime,
        Graal.getRequiredCapability(classOf[Replacements]),
        backend,
        target,
        cache,
        plan,
        OptimisticOptimizations.NONE,
        new SpeculationLog(),
        Suites.createDefaultSuites(),
        new CompilationResult()
      )

      if (true) {
        printGraph("Final")
      }
      assert(res != null);
      Predef.println("Jit compilation time t = " + (System.currentTimeMillis - start)+ "ms")

      res
    }

    val resMethod = runtime.addMethod(method, result, null)
    assert(resMethod != null);
    resMethod
  }

  def compile[A: Manifest, Ret: Manifest]: A => Ret = {

    val compiledMethod = compile0(cls)

    { (x:A) =>
      val res = compiledMethod.executeVarargs(().asInstanceOf[AnyRef], x.asInstanceOf[AnyRef])
      res.asInstanceOf[Ret]
    }
  }

  def compile2[A: Manifest, B: Manifest, Ret: Manifest]: (A, B) => Ret = {

    val compiledMethod = compile0(cls)

    { (p1:A, p2: B) =>
      val res = compiledMethod.executeVarargs(().asInstanceOf[AnyRef], p1.asInstanceOf[AnyRef], p2.asInstanceOf[AnyRef])
      res.asInstanceOf[Ret]
    }
  }

  def compile3[A: Manifest, B: Manifest, C: Manifest, Ret: Manifest]: (A, B, C) => Ret = {

    val compiledMethod = compile0(cls)

    { (p1:A, p2: B, p3: C) =>
      val y = compiledMethod.executeVarargs(().asInstanceOf[AnyRef], p1.asInstanceOf[AnyRef], p2.asInstanceOf[AnyRef], p3.asInstanceOf[AnyRef])
      y.asInstanceOf[Ret]
    }
  }

  def compile4[A: Manifest, B: Manifest, C: Manifest, D: Manifest, Ret: Manifest]: (A, B, C, D) => Ret = {

    val compiledMethod = compile0(cls)

    { (p1:A, p2: B, p3: C, p4: D) =>
      val y = compiledMethod.executeVarargs(().asInstanceOf[AnyRef], p1.asInstanceOf[AnyRef], p2.asInstanceOf[AnyRef], p3.asInstanceOf[AnyRef], p4.asInstanceOf[AnyRef])
      y.asInstanceOf[Ret]
    }
  }

  // --------- Util

  def topScope[A](method: ResolvedJavaMethod)(body: => A) = {
    import GraalDebugConfig._
    val hotspotDebugConfig =
      new GraalDebugConfig(
       Log.getValue(),
       Meter.getValue(),
       Time.getValue(),
       Dump.getValue(),
       "graal_1-new.apply",// MethodFilter.getValue()
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

  def printGraphPhase(s: String, verbosity: Node.Verbosity = Node.Verbosity.Short) = phase { graph =>
    println("===== " + s)
    graph.getNodes.foreach(n => println(n.toString(verbosity) + n.inputs().map(_.toString(Node.Verbosity.Id)).mkString("(",",",")")))
    println("----- " + s + " method calls ")
    graph.getNodes(classOf[InvokeNode]).foreach(printInvoke)
  }

  def printGraph(s: String, verbosity: Node.Verbosity = Node.Verbosity.Short) =
    graph.getNodes.foreach(n => println(n.toString(Node.Verbosity.Debugger) + n.inputs().map(_.toString(Node.Verbosity.Id)).mkString("(",",",")")))

  def printInvoke(invoke: InvokeNode): Unit = {
    if (invoke.callTarget.isInstanceOf[MethodCallTargetNode]) {
      val methodCallTarget = invoke.callTarget()

      println("  invoke: " + invoke)
      println("    args: " + methodCallTarget.arguments())

      val assumptions = new Assumptions(true)

      // val info = InliningUtil.getInlineInfo(methodCallTarget.invoke(), assumptions, OptimisticOptimizations.ALL)
      // println("    info: " + info)
    } else {
      println("Invoke Node: " + invoke)
    }
  }

  def phase(f: StructuredGraph => Unit) = new Phase {
    def run(graph: StructuredGraph) = f(graph)
  }

}

trait GraalGenBase extends BlockTraversal {
  val IR: Expressions
  import IR._

  val runtime = HotSpotGraalRuntime.graalRuntime().getRuntime();
  val config = GraphBuilderConfiguration.getEagerDefault()

  // graal stuff imported
  val graphBuilder: LancetGraphBuilder = new LancetGraphBuilder(runtime, config, OptimisticOptimizations.ALL)
  def graph: StructuredGraph = graphBuilder.currentGraph

 def resetGenerator(): Unit = {}

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

  def genClassName() = "graal_" + GEN_Graal_LMS.classCounter.incrementAndGet

  // ----------

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) =>
      emitNode(sym,rhs)
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
  def emit[A : Manifest](args: List[Sym[_]], body: Block[A], className: String = genClassName()): List[(Sym[Any], Any)]

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

trait GraalBuilderNested extends GraalBuilder { self: GraalNestedCodegen =>
  val IR: Expressions with Effects
  import IR._
  import graphBuilder._

  def while_g(condition: Block[Boolean], body: Block[Unit]): Unit = {
    while_g({
      emitBlock(condition)
      push(condition.res)
    },
      emitBlock(body)
    )
  }

  def if_g[T](c: Exp[Boolean], thenBlock: Block[T], elseBlock: Block[T]): Unit = {
    push(c, Const(0))

    if_g(Condition.EQ, {
      emitBlock(elseBlock)
      push(elseBlock.res)
    }, {
      emitBlock(thenBlock)
      push(thenBlock.res)
    })
  }

}

trait GraalBuilder extends GraalGenBase {
  import IR._
  import graphBuilder._

  override def resetGenerator(): Unit = {
    super.resetGenerator()
    localsPos.clear()
    localsSize = 1
  }

  // separate from localsPos since Double and Long take 2 spots
  // starts from 1 because of the `this` pointer
  var localsSize: Int = 1
  val localsPos: MMap[Sym[Any], Int] = new MHashMap()

  def tpString(e: Exp[Any]): String = e match {
    case _ if e.tp.erasure == classOf[Variable[Any]] => e.tp.typeArguments.head.toString
    case _ => e.tp.toString
  }

  def kind(e: Exp[Any]): Kind = kind(tpString(e))

  def kind(e: String): Kind = e match {
    case "Boolean"=> Kind.Boolean
    case "Byte"   => Kind.Byte
    case "Char"   => Kind.Char
    case "Short"  => Kind.Short
    case "Int"    => Kind.Int
    case "Long"   => Kind.Long
    case "Float"  => Kind.Float
    case "Double" => Kind.Double
    case "Unit"   => Kind.Void
    case _        => Kind.Object
  }

  def operation(sym: Sym[_])(node: Seq[ValueNode] => ValueNode): Unit = {
    insert(sym)
    val operands = rsyms(findDefinition(sym).get.rhs)(x => x.asInstanceOf[Exp[_]] :: Nil).toSeq.reverse
    // push all operands on the stack
    push(operands:_*)
    val valueNodes = operands map { x => frameState.pop(kind(x)) }
    frameState.push(kind(sym), node(valueNodes))

    storeLocal(kind(sym), lookup(sym))
  }

  def push(exps: Exp[Any]*): Unit = exps foreach {
    case Const(v: Int) =>
      frameState.ipush(ConstantNode.forConstant(Constant.forInt(v), runtime, graph))
    case Const(v: Long) =>
      frameState.lpush(ConstantNode.forConstant(Constant.forLong(v), runtime, graph))
    case Const(v: Float) =>
      frameState.fpush(ConstantNode.forConstant(Constant.forDouble(v), runtime, graph))
    case Const(v: Double) =>
      frameState.dpush(ConstantNode.forConstant(Constant.forDouble(v), runtime, graph))
    case Const(v: Boolean) =>
      frameState.ipush(ConstantNode.forConstant(Constant.forBoolean(v), runtime, graph))
    case Const(v: Unit) =>
      frameState.apush(ConstantNode.forConstant(Constant.forObject(()), runtime, graph))
    case Const(v: AnyRef) =>
      frameState.apush(ConstantNode.forConstant(Constant.forObject(v), runtime, graph))
    case sym@Sym(v) if sym.tp.toString == "Unit" =>
      frameState.apush(ConstantNode.forConstant(Constant.forObject(()), runtime, graph))
    case sym@Sym(v) =>
      val loc = frameState.loadLocal(lookup(sym))
      frameState.push(kind(sym), loc)
  }

  def lookup(s: Sym[Any]): Int = {
    require(localsPos.contains(s), s"Symbol ($s) must have a stack position allocated before its usage. Use function insert(s: Sym[Any]).")
    localsPos(s)
  }

  def insert(s: Sym[Any], pos: Sym[Any]): Unit = if(!localsPos.contains(s)) localsPos += (s -> lookup(pos))

  def insert(s: Sym[Any]): Unit = tpString(s) match {
    case "Double" | "Long" =>
      localsPos  += (s -> localsSize)
      localsSize += 2
    case _ =>
      localsPos  += (s -> localsSize)
      localsSize += 1
  }

  // TODO see if this is dead code
  def clearLocals(fs: FrameStateBuilder)(locals: Int*) = {
    val removeLocals = new BitSet()
    locals.foreach(removeLocals.set(_))
    fs.clearNonLiveLocals(removeLocals)
  }

  def invokeStatic(clazz: Class[_], methodName: String, resType: Class[_], argTypes: Class[_]*) = {
    val reflMethod = clazz.getDeclaredMethod(methodName, argTypes:_*);
    val resolvedMethod = runtime.lookupJavaMethod(reflMethod)

    val currentCall = JVMMethodCall(Opcodes.INVOKESTATIC, clazz.getName.replaceAll("\\.", "/"), methodName, argTypes.toList, resType)
    currentCalls += currentCall
    val bci = if(methodCalls != null)
      methodCalls.takeWhile(x => x != currentCall)
        .foldLeft(0)((x,y) => x + 1 + y.args.size + 4) + (1 + argTypes.size)
    else 0

    stream.setBCI(bci)

    genInvokeStatic(resolvedMethod)

    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))

    // block stuff
    val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
    val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState)
    val result = target.fixed
    val tmpState = frameState.copy()
    appendGoto(result)

    frameState = tmpState
    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    lastInstr = nextFirstInstruction
  }

  def methodCalls: Seq[JVMMethodCall]
  def currentCalls: ArrayBuffer[JVMMethodCall]
  def invoke(clazz: Class[_], methodName: String, retType: Manifest[_], argTypes: Manifest[_]*): Unit =
    invoke(clazz, methodName, retType.runtimeClass, argTypes.map(_.runtimeClass):_*)

  def invoke(clazz: Class[_], methodName: String, retType: Class[_], argTypes: Class[_]*): Unit = {
    val reflMethod = clazz.getDeclaredMethod(methodName, argTypes:_*);
    val resolvedMethod = runtime.lookupJavaMethod(reflMethod)
    val currentCall = JVMMethodCall(Opcodes.INVOKEVIRTUAL, clazz.getName.replaceAll("\\.", "/"), methodName, argTypes.toList, retType)
    currentCalls += currentCall
    val bci = if(methodCalls != null)
      methodCalls.takeWhile(x => x != currentCall)
        .foldLeft(0)((x,y) => x + 1 + y.args.size + 4) + (1 + argTypes.size)
    else 0
    Predef.println("Virtual bci: " + bci)
    stream.setBCI(bci)
    // graalArgs
    genInvokeSpecial(resolvedMethod)
    // genInvokeIndirect(MethodCallTargetNode.InvokeKind.Virtual, resolvedMethod, graalArgs)
    lastInstr.asInstanceOf[StateSplit].setStateAfter(frameState.create(0))

    // block stuff
    val nextFirstInstruction = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
    val target = new LancetGraphBuilder.Target(nextFirstInstruction, frameState)
    val result = target.fixed
    val tmpState = frameState.copy()
    appendGoto(result)

    frameState = tmpState
    frameState.cleanupDeletedPhis();
    frameState.setRethrowException(false);
    lastInstr = nextFirstInstruction
  }

  def pushToString(exps: Exp[_]*) = exps.foreach { s =>
    push(s)
    tpString(s) match {
      case  "java.lang.String" =>
      case "Char" =>
        invokeStatic(classOf[_root_.java.lang.Character], "toString", classOf[String], classOf[Char])
      case "Int" =>
        invokeStatic(classOf[Integer], "toString", classOf[String], classOf[Int])
      case "Long" =>
        invokeStatic(classOf[_root_.java.lang.Long], "toString", classOf[String], classOf[Long])
      case "Double" =>
        invokeStatic(classOf[_root_.java.lang.Double], "toString", classOf[String], classOf[Double])
      case "Float" =>
        invokeStatic(classOf[_root_.java.lang.Float], "toString", classOf[String], classOf[Float])
      case "Boolean" =>
        invokeStatic(classOf[_root_.java.lang.Boolean], "toString", classOf[String], classOf[Boolean])
      case "AnyRef" =>
        invoke(s.tp.runtimeClass, "toString", classOf[String], classOf[String])
  }}

  def ssa(sym: Sym[_])(block: =>Unit) = {
    insert(sym)
    block
    if (tpString(sym) != "Unit") storeLocal(kind(sym), lookup(sym))
  }

  def convert(sym: Sym[_], lhs: Exp[_], op: ConvertNode.Op) = {
    insert(sym)
    push(lhs)
    genConvert(op)
    storeLocal(kind(sym), lookup(sym))
  }

  /**
   * To use this impure method push two ints in the fifo order to the frameState.
   */
  def if_g(c: Condition, thenBlock: => Unit, elseBlock: => Unit): Unit = {
    val (rhs, lhs) = (frameState.pop(Kind.Int), frameState.pop(Kind.Int))
    val ((thn, frameStateThen), (els, frameStateElse)) = ifNode(lhs, c, rhs, null)
    lastInstr = thn
    frameState = frameStateThen

    // [then] (NOTE: That is how scala and java do it. Yes else goes into the then block.)
    thenBlock
    // [then]

    var exitState = frameState.copy()
    val target = currentGraph.add(new LancetGraphBuilder.BlockPlaceholderNode())
    appendGoto({ // inlined create target
     val result = new LancetGraphBuilder.Target(target, frameState);
     result.fixed
    })

    lastInstr = els
    frameState = frameStateElse

    // [else] (NOTE: That is how scala and java do it. Yes else goes into the then block.)
    elseBlock
    // [else]

    // The EndNode for the already existing edge.
    val end = currentGraph.add(new EndNode())
    // The MergeNode that replaces the placeholder.
    val mergeNode = currentGraph.add(new MergeNode());
    appendGoto({ // inlined create target
      val next = target.next();

      target.setNext(end);
      mergeNode.addForwardEnd(end);
      mergeNode.setNext(next);

      // The EndNode for the newly merged edge.
      val newEnd = currentGraph.add(new EndNode())
      val target2 = new LancetGraphBuilder.Target(newEnd, frameState);
      val result = target2.fixed;
      exitState.merge(mergeNode, target2.state);
      mergeNode.addForwardEnd(newEnd);
      result
    })
    frameState = exitState
    lastInstr = mergeNode
    mergeNode.setStateAfter(frameState.create(0))
  }

  def while_g(condition: => Unit, whileBody: => Unit): Unit = {
    val preLoopEnd = currentGraph.add(new EndNode())
    val loopBegin = currentGraph.add(new LoopBeginNode())
    lastInstr.setNext(preLoopEnd)
    // Add the single non-loop predecessor of the loop header.
    loopBegin.addForwardEnd(preLoopEnd)
    lastInstr = loopBegin

    // Create phi functions for all local variables and operand stack slots.
    frameState.insertLoopPhis(loopBegin)
    loopBegin.setStateAfter(frameState.create(0))

    val loopFristInstr = loopBegin
    val loopBlockState = frameState.copy()

    frameState = loopBlockState
    lastInstr = loopBegin
    frameState.cleanupDeletedPhis();

    condition

    val ((thn, frameStateThen), (els, frameStateElse)) =
      ifNode(frameState.pop(Kind.Int), Condition.EQ, appendConstant(Constant.INT_0), (loopBegin, loopBlockState));

    // starting the body (else block)
    frameState = frameStateElse // should the loop block state go here?
    lastInstr = els
    frameState.cleanupDeletedPhis();

    whileBody

    appendGoto({
      val target = new LancetGraphBuilder.Target(currentGraph.add(new LoopEndNode(loopBegin)), frameState)
      val result = target.fixed
      loopBlockState.merge(loopBegin, target.state)
      result
    })

    // after loop (then block)
    frameState = frameStateThen
    lastInstr = thn
  }
}

trait GraalCodegen extends GraalGenBase with GraalBuilder

trait GraalNestedCodegen extends GraalGenBase with NestedBlockTraversal with GraalBuilderNested {
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

  override def push(exps: Exp[Any]*): Unit = exps foreach {
    case Def(Reify(s, u, effects)) =>
      push(s)
    case exp => super.push(exp)
  }
}

case class GraalBackendException(m: String) extends Exception(m)