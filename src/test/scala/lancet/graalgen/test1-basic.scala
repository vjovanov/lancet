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
import lancet.core._

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime
import scala.virtualization.lms.common._
import scala.virtualization.lms.util.OverloadHack

trait DSL extends ScalaOpsPkg with TupledFunctions with UncheckedOps with LiftPrimitives with LiftString with LiftVariables
{
  def main(arg: Rep[Int]): Rep[Int]
}

trait Impl extends DSL with ScalaOpsPkgExp with TupledFunctionsRecursiveExp with UncheckedOpsExp
  { self =>
  val start = System.currentTimeMillis
  val codegen = new GEN_Graal_LMS with GraalGenPrimitiveOps with GraalGenIfThenElse
    with GraalGenOrderingOps with GraalGenVariables with GraalGenWhile with GraalGenArrayOps
    with GraalGenEqual with GraalGenStringOps with GraalGenIOOps with GraalGenMiscOps
    with GraalGenNumericOps
    { val IR: self.type = self

    emit(main)
  }
  val function = codegen.compile[Int, Int]
  Predef.println("Code generation time t = " + (System.currentTimeMillis - start))
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

  def testRet = withOutFile(prefix+"-ret") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = v
    }
    val f = (new Prog with Impl).function
    assert(f(1) == 1)
    assert(f(2) == 2)
  }

  def testInc = withOutFile(prefix+"-inc") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = v + 1
    }
    val f = (new Prog with Impl).function
    assert(f(1) == 2)
    assert(f(2) == 3)
  }

  def testArith = withOutFile(prefix+"-arith") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = v + 1 + v * 2
    }
    val f = (new Prog with Impl).function
    assert(f(1) == 4)
    assert(f(2) == 7)
  }

  def testIf = withOutFile(prefix+"-if") {
    trait Prog extends DSL {
      def main(x: Rep[Int]): Rep[Int] = {
        if (x < 0) x + 42 else x - 42
      }
    }
    val f = (new Prog with Impl).function

    assert(f(10) == -32)
    assert(f(-42) == 0)
    assert(f(42) == 0)
    assert(f(0) == -42)
  }

  def testNestedIf = withOutFile(prefix+"-nestedif") {
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

  def testWhile = withOutFile(prefix+"-while") {
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

  def testNestedWhile = withOutFile(prefix+"-nestedwhile") {
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

  def testWhileIfWhile = withOutFile(prefix+"-whileifwhile") {
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
    (0 to 10) foreach { x => Predef.println(f(x)) }
    (0 to 10) foreach { x =>
      val (v1, v2) = (0 until x).partition(_ % 2 == 1)
      assert(f(x) == v1.flatMap(i => (0 until i)).sum + v2.flatMap(i => (0 until i)).sum * 2)
    }
  }

  def testArrays = withOutFile(prefix+"-arrays") {
    trait Prog extends DSL {
      def main(x: Rep[Int]): Rep[Int] = {
        var arr: Rep[Array[Int]] = NewArray[Int](x)
        var i = 0
        val res = while (i < x) {
          arr(i) = x
          i = i + 1
          ()
        }
        if (x > 0) arr(0) else x
      }
    }
    val f = (new Prog with Impl).function
    (5 to 10) foreach { x => assert(f(x) == x) }
  }

  def testFunctions = withOutFile(prefix+"-functions") {
    trait Prog extends DSL {
      def main(x: Rep[Int]): Rep[Int] = {
        println("Arg = " + x.toString)
        1
      }
    }
    val f = (new Prog with Impl).function
    withOutFileChecked(prefix+"-functions-output") {
      assert(f(5) == 1)
      assert(f(6) == 1)
    }
  }

}
