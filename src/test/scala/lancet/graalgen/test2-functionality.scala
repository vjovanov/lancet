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

class TestFunctionality extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-functionality"

  /*def testPrinting = withOutFile(prefix+"-print") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = {
        val i: Rep[Int] = v
        val l: Rep[Long] = v.toLong
        val f: Rep[Float] = v.toFloat
        val d: Rep[Double] = v.toDouble
        println(i)
        println("Int Value = " + i)
        println(l)
        println("Long Value = " + l)
        println(f)
        println("Long Value = " + f)
        println(d)
        println("Double Value = " + d)
        v
      }
    }

    val f = (new Prog with Impl).function
    withOutFileChecked(prefix+"-print-o"){
      f(1)
    }
    }
  }*/

  def testArithmetics = withOutFile(prefix+"-arithmetics") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = {
        val i: Rep[Int] = v
        val d: Rep[Double] = v.toDouble
        val f: Rep[Float] = v.toFloat
        println("Int Arithmetics:")
        println(i + 1) // 2
        println(1 + i) // 2
        println("Double Arithmetics:")
        println(i + d) // 2.0D
        v
      }
    }

    val f = (new Prog with Impl).function
    withOutFileChecked(prefix+"-arithmetics-out"){
      f(1)
    }
  }

  def testConditions = withOutFile(prefix + "-conditions") {
    trait Prog extends DSL {
      def main(v: Rep[Int]): Rep[Int] = {
        val i: Rep[Int] = v
        // val f: Rep[Float] = v.toFloat
        // val d: Rep[Double] = v.toDouble
        // Int
        println("Int:")
        if(i > i - 1) println("OK!") else println("ERROR!")
        if(i > i) println("ERROR!") else println("OK!")
        if(i < i + 1) println("OK!") else println("ERROR!")
        if(i < i) println("ERROR!") else println("OK!")
        if(i >= i) println("OK!") else println("ERROR!")
        if(i >= i + 1) println("ERROR!") else println("OK!")
        if(i <= i) println("OK!") else println("ERROR!")
        if(i <= i - 1) println("ERROR!") else println("OK!")

        // Double
        // This fails with because of nulls in the frameStateBuilder. Where do they come from???
        // val x = d < d
        // val y = d > d
        // println(x)
        // println(y)
        v
      }
    }

    val f = (new Prog with Impl).function
    withOutFileChecked(prefix+"-conditions-out"){
      f(1)
    }
  }

}
