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

import com.oracle.graal.api.meta._      // ResolvedJavaMethod
import com.oracle.graal.hotspot._
import com.oracle.graal.hotspot.meta._  // HotSpotRuntime


trait GraalGenBase {
  def newInterpreter = new BytecodeInterpreter_LMS_Opt with Compiler {
    initialize()
    debugBlockKeys = false
    def compile[A:Manifest,B:Manifest](f: A => B): A=>B = compile0(f)
  }
}

class TestGraalGenBasic extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-basic-"

  // interpret
  def testInc = withOutFileChecked(prefix+"inc") {

    val it = newInterpreter
    it.TRACE = true
    it.TRACE_BYTE_CODE = true

    val res = it.compile { (x:Int) => x + 1 }

    println("res: " + res)
  }
}