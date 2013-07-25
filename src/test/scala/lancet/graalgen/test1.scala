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
    val codegen = new GEN_Graal_LMS with GraalGenPrimitiveOps with GraalGenIfThenElse with GraalGenOrderingOps with GraalGenVariables with GraalGenWhile { val IR: self.type = self
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
        params(tmp5,tmp4,tmp3)
        tmp7
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
  /*def testInc = withOutFileChecked(prefix+"-inc") {

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
      assert(f(-42) == 0)
      assert(f(42) == 0)
      assert(f(0) == 42)
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
      assert(f(-43) == -11)
      assert(f(-0) == -12)
      assert(f(19) == -2)
      assert(f(21) == -1)
      assert(f(42) == 20)
    }
  }*/

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
      (0 to 100) foreach { x => assert(f(x) == (x * (x + 1) / 2)) }
    }
  }

}