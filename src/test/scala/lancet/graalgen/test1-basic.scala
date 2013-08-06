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
  with VectArrayOpsExp
  { self =>
  def params(i: Int, b: Int, c: Int) = ???

  val codegen = new GEN_Graal_LMS with GraalGenPrimitiveOps with GraalGenIfThenElse
    with GraalGenOrderingOps with GraalGenVariables with GraalGenWhile with GraalGenArrayOps
    with GraalGenEqual with GraalGenStringOps with GraalGenIOOps with GraalGenMiscOps
    with GraalGenNumericOps
    { val IR: self.type = self

    val f = (x: Int) => { // TODO this is needed for now to trick the FrameStateBuilder.
      val tmp0       = x
      val tmp1      =      tmp0  + 1
      val tmp2      =      tmp1  + 1
      val tmp3      =      tmp2  + 1
      val tmp4      =      tmp3  + 1
      val tmp5      =      tmp4  + 1
      val tmp6      =      tmp5  + 1
      val tmp7      =      tmp6  + 1
      val tmp8      =      tmp7  + 1
      val tmp9      =      tmp8  + 1
      val tmp10     =      tmp9  + 1
      val tmp11     =      tmp10 + 1
      val tmp12     =      tmp11 + 1
      val tmp13     =      tmp12 + 1
      val tmp14     =      tmp13 + 1
      val tmp15     =      tmp14 + 1
      val tmp16     =      tmp15 + 1
      val tmp17     =      tmp16 + 1
      val tmp18     =      tmp17 + 1
      val tmp19     =      tmp18 + 1
      val tmp20     =      tmp19 + 1
      val tmp21     =      tmp20 + 1
      val tmp22     =      tmp21 + 1
      val tmp23     =      tmp22 + 1
      val tmp24     =      tmp23 + 1
      val tmp25     =      tmp24 + 1
      val tmp26     =      tmp25 + 1
      val tmp27     =      tmp26 + 1
      val tmp28     =      tmp27 + 1
      val tmp29     =      tmp28 + 1
      val tmp30     =      tmp29 + 1
      val tmp31     =      tmp30 + 1
      val tmp32     =      tmp31 + 1
      val tmp33     =      tmp32 + 1
      val tmp34     =      tmp33 + 1
      val tmp35     =      tmp34 + 1
      val tmp36     =      tmp35 + 1
      val tmp37     =      tmp36 + 1
      val tmp38     =      tmp37 + 1
      val tmp39     =      tmp38 + 1
      val tmp40     =      tmp39 + 1
      val tmp41     =      tmp40 + 1
      val tmp42     =      tmp41 + 1
      val tmp43     =      tmp42 + 1
      val tmp44     =      tmp43 + 1
      val tmp45     =      tmp44 + 1
      val tmp46     =      tmp45 + 1
      val tmp47     =      tmp46 + 1
      val tmp48     =      tmp47 + 1
      val tmp49     =      tmp48 + 1
      val tmp50     =      tmp49 + 1
      val tmp51     =      tmp50 + 1
      val tmp52     =      tmp51 + 1
      val tmp53     =      tmp52 + 1
      val tmp54     =      tmp53 + 1
      val tmp55     =      tmp54 + 1
      val tmp56     =      tmp55 + 1
      val tmp57     =      tmp56 + 1
      val tmp58     =      tmp57 + 1
      val tmp59     =      tmp58 + 1
      val tmp60     =      tmp59 + 1
      val tmp61     =      tmp60 + 1
      val tmp62     =      tmp61 + 1
      val tmp63     =      tmp62 + 1
      val tmp64     =      tmp63 + 1
      val tmp65     =      tmp64 + 1
      val tmp66     =      tmp65 + 1
      val tmp67     =      tmp66 + 1
      val tmp68     =      tmp67 + 1
      val tmp69     =      tmp68 + 1
      val tmp70     =      tmp69 + 1
      val (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15, b16, b17, b18, b19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15, e16, e17, e18, e19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16, g17, g18, g19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, h14, h15, h16, h17, h18, h19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15, i16, i17, i18, i19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, j14, j15, j16, j17, j18, j19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      val (k1, k2, k3, k4, k5, k6, k7, k8, k9, k10, k11, k12, k13, k14, k15, k16, k17, k18, k19) = (1,2,3,4,5, 1,2,3,4,5, 1,2,3,4,5, 1,2,3,4)
      params(tmp5,tmp4,tmp40)
      tmp70
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
