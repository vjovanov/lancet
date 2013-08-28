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

import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

class TestBytecodeTemplates extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-functionx"

  def testFunction3 = withOutFile(prefix+"-f3") {
    val f3Template = new FunctionTemplate("f3",
      List(manifest[Int], manifest[Double], manifest[Long]),
      manifest[Boolean],
      600,
      600,
      Nil
    )
    val classBytes = f3Template.bytecode
    val vfs = new VirtualDirectory("<vfs>", None)
    val file = vfs.fileNamed("f3.class")
    val bout = file.bufferedOutput
    bout.write(classBytes, 0, classBytes.length)
    bout.flush
    bout.close()
    val loader = new AbstractFileClassLoader(vfs, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass("f3")
    val cons = cls.getConstructor()
    val f3 = cons.newInstance().asInstanceOf[(Int, Double, Long)=>Boolean]
    println(f3(1, 1.0D, 1L))
  }

  def testFunction8 = withOutFile(prefix+"-f8") {
    val f8Template = new FunctionTemplate("f8",
      List(manifest[Char], manifest[Short], manifest[Int], manifest[Long], manifest[Float], manifest[Double], manifest[Boolean], manifest[Array[Any]]),
      manifest[Boolean],
      600,
      600,
      Nil
    )
    val classBytes = f8Template.bytecode
    val vfs = new VirtualDirectory("<vfs>", None)
    val file = vfs.fileNamed("f8.class")
    val bout = file.bufferedOutput
    bout.write(classBytes, 0, classBytes.length)
    bout.flush
    bout.close()
    val loader = new AbstractFileClassLoader(vfs, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass("f8")
    val cons = cls.getConstructor()
    val f8 = cons.newInstance().asInstanceOf[(Char, Short, Int, Long, Float, Double, Boolean, Array[Any])=>Boolean]
    println(f8('a', 1, 1, 1L, 1.0F, 1.0D, true, Array[Any](1)))
  }

    def testStubs = withOutFile(prefix+"-stub") {
      val f3Template = new FunctionTemplate("f3",
        List(manifest[Int], manifest[Double], manifest[Long]),
        manifest[Boolean],
        600,
        600,
        List(("scala/Predef$", "println", manifest[AnyRef] :: Nil, manifest[Unit]))
      )
      val classBytes = f3Template.bytecode
      // positions
      val vfs = new VirtualDirectory("<vfs>", None)
      val file = vfs.fileNamed("f3.class")
      val bout = file.bufferedOutput
      bout.write(classBytes, 0, classBytes.length)
      bout.flush
      bout.close()
      val loader = new AbstractFileClassLoader(vfs, this.getClass.getClassLoader)
      val cls: Class[_] = loader.loadClass("f3")
      val cons = cls.getConstructor()
      val f3 = cons.newInstance().asInstanceOf[(Int, Double, Long)=>Boolean]
  }
}
