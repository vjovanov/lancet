/*
 * Copyright (c) 2013 Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see http://www.gnu.org/licenses/agpl.html.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package lancet
package graalgen

import lancet.api._
import lancet.interpreter._
import lancet.codegen._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

trait DSL extends ScalaOpsPkg with TupledFunctions with UncheckedOps with LiftPrimitives with LiftString with LiftVariables {
  def main(arg: Rep[Int]): Rep[Int]
}

trait Impl extends DSL with ScalaOpsPkgExp with TupledFunctionsRecursiveExp with UncheckedOpsExp { self =>
    def params(i: Int, b: Int, c: Int) = ???
    val codegen = new GEN_Graal_LMS with GraalGenPrimitiveOps with GraalGenIfThenElse with GraalGenOrderingOps with GraalGenVariables with GraalGenWhile with GraalGenEqual { val IR: self.type = self
      val f = (x: Int) => { // TODO this is needed for now to trick the FrameStateBuilder.
        val tmp = x
        val tmp1 = tmp + 1
        val tmp2 = tmp1 + 1
        val tmp3 = tmp2 + 1
        val tmp4 = tmp3 + 1
        val tmp5 = tmp4 + 1
        val tmp6 = tmp5 + 1
        val tmp7 = tmp6 + 1
        val tmp8 = tmp7 + 1
        val tmp9 = tmp8 + 1
        val tmp10 = tmp9 + 1
        val tmp11 = tmp10 + 1
        val tmp12 = tmp11 + 1
        val tmp13 = tmp12 + 1
        val tmp14 = tmp13 + 1
        val tmp15 = tmp14 + 1
        val tmp16 = tmp15 + 1
        val tmp17 = tmp16 + 1
        val tmp18 = tmp17 + 1
        val tmp19 = tmp18 + 1
        val tmp20 = tmp19 + 1
        val tmp21 = tmp20 + 1
        val tmp22 = tmp21 + 1
        val (t1, t2, t3, t4, t5) = (1,2,3,4,5)
        params(tmp5,tmp4,tmp3)
        tmp14
      }
      val input = fresh[Int]
      val cls = f.getClass
      val reflectMeth = cls.getDeclaredMethod("apply$mcII$sp", classOf[Int])
      val method = runtime.lookupJavaMethod(reflectMeth)
      val block = reifyBlock{main(input)}
      emit(scala.List(input), block, method)
    }

    val function = codegen.compile(codegen.f)
  }


trait GraalGenBase {
  def newInterpreter = new BytecodeInterpreter_LMS_Opt with Compiler {
    initialize()
    debugBlockKeys = false
    def compile[A:Manifest,B:Manifest](f: A => B): A=>B = compile0(f)
  }
}

class TestGraalGenBasic extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-basic"

  // interpret
  def testInc = withOutFileChecked(prefix+"-inc") {

    withOutFile(prefix+"-inc") {
      trait Prog extends DSL {
        def main(v: Rep[Int]): Rep[Int] = v + 38 + v * 4 - v
      }
      val f = (new Prog with Impl).function
      println(f(1))
      println(f(2))
      assert(f(1) == 42)
      assert(f(2) == 46)
    }
  }

  def testIf = withOutFileChecked(prefix+"-if") {

    withOutFile(prefix+"-if") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] = if (x < 0) x + 42 else x - 42
      }
      val f = (new Prog with Impl).function
      assert(f(10) == -32)
      assert(f(-42) == 0)
      assert(f(42) == 0)
      assert(f(0) == -42)
    }
  }

  def testNestedIf = withOutFileChecked(prefix+"-nestedif") {

    withOutFile(prefix+"-nestedif") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] =
          if (x < 00)
            if(x < -10) -11 else -12
          else
            if (x > 20) x - 22 else x - 21
      }
      val f = (new Prog with Impl).function
      assert(f(-10) == -12)
      assert(f(-11) == -11)
      assert(f(20) == -1)
      assert(f(40) == 18)

    }
  }

  def testWhile = withOutFileChecked(prefix+"-while") {

    withOutFile(prefix+"-while") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] = {
          var sum = 0
          var i = 0
          val res = while (i < x) {
            sum = sum + i
            i = i + 1
            ()
          }
          sum
        }
      }
      val f = (new Prog with Impl).function
      (0 to 100) foreach { x => println(f(x)) }
    }
  }

  def testNestedWhile = withOutFileChecked(prefix+"-nestedwhile") {
    withOutFile(prefix+"-nestedwhile") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] = {
          var sum = 0
          var i = 0
          while (i < x) {
            var j = 0
            while(j < i) {
              sum = sum + j
              j = j + 1
            }
            i = i + 1
          }
          sum
        }
      }
      val f = (new Prog with Impl).function
      (0 to 100) foreach { x => Predef.println(f(x)) }
      (0 to 100) foreach { x => assert(f(x) == (0 until x).map(i => (0 until i).sum).sum) }
    }
  }

  def testWhileIfWhile = withOutFileChecked(prefix+"-whileifwhile") {
    withOutFile(prefix+"-whileifwhile") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] = {
          var sum = 0
          var i = 0
          while (i < x) {
            var j = 0
            if (i % 2 == 1) {
              while(j < i) {
                sum = sum + j
                j = j + 1
              }
            } else {
              while(j < i) {
                sum = sum + (j * 2)
                j = j + 1
              }
            }

            i = i + 1
          }
          sum
        }
      }
      val f = (new Prog with Impl).function
      (0 to 100) foreach { x => Predef.println(f(x)) }
      (0 to 100) foreach {x =>
        val (v1, v2) = (0 until x).partition(_ % 2 == 1)
        assert(f(x) == v1.flatMap(i => (0 until i)).sum + v2.flatMap(i => (0 until i)).sum * 2)
      }
    }
  }

  def testArrays = withOutFileChecked(prefix+"-while") {

    withOutFile(prefix+"-while") {
      trait Prog extends DSL {
        def main(x: Rep[Int]): Rep[Int] = {
          var arr: Rep[Array[Int]] = NewArray[Int](x)
          var i = 0
          val res = while (i < x) {
            arr(i) = x
            i = i + 1
            ()
          }
          x
        }
      }
      val f = (new Prog with Impl).function
      (0 to 100) foreach { x => assert(f(x) == (x * (x + 1) / 2)) }
    }
  }

}