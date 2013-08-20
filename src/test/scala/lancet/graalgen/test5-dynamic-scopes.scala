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

trait UnrollOps extends Base {
  def unroll[T](b: => Rep[T]): Rep[T]
}

trait Unroll extends UnrollOps with DynamicScopes {
  case object UnrollLoop extends DynamicScope
  def unroll[T](b: => Exp[T]): Exp[T] = dynamicScope(UnrollLoop)(b)
}

class TestDynamicScopes extends FileDiffSuite with GraalGenBase  {

  val prefix = "test-out/test-dynamic-scopes"

  def testUnroll = withOutFile(prefix+"-unroll") {
    trait Prog extends DSL with  UnrollOps {
      def main(v: Rep[Int]): Rep[Int] = {
        unroll {
          var i = 0
          while(i < 10) {
            println(i)
            i = i + 1
          }
          var j = 0
          while(j < v) {
            println(j * 100)
            j = j + 1
          }
        }
        v
      }
    }

    val f = (new Prog with Impl with Unroll {
      Predef.println("Symbol scopes = " + symbolScopes.mkString)
    }).function
    withOutFileChecked(prefix+"-unroll-out") {
      f(1)
    }
  }
}
