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
import lancet.core._

trait KMeansImpl extends DSL with ScalaOpsPkgExp with TupledFunctionsRecursiveExp with UncheckedOpsExp
  { self =>
  def params(i: Int, b: Int, c: Int) = ???
  val codegen = new GEN_Graal_LMS with GraalGenPrimitiveOps with GraalGenIfThenElse
    with GraalGenOrderingOps with GraalGenVariables with GraalGenWhile with GraalGenArrayOps
    with GraalGenEqual with GraalGenStringOps with GraalGenIOOps with GraalGenMiscOps
    with GraalGenNumericOps with GraalGenMathOps with GraalGenImplicitOps { val IR: self.type = self

    val (c, cCol, p, pCol) = (fresh[Array[Double]], fresh[Int], fresh[Array[Double]], fresh[Int])
    val block = reifyBlock{main(c, cCol, p, pCol)}
    emit(scala.List(c, cCol, p, pCol), block)
  }

  def main(c: Rep[Array[Double]], cCol:Rep[Int], p:Rep[Array[Double]], pcol:Rep[Int]): Rep[Array[Double]]

  val function = codegen.compile4[Array[Double], Int, Array[Double], Int, Array[Double]]
}

trait NearestCluster extends DSL {
  def constructNearestClusterVector(v: Rep[Array[Int]], x: Rep[Array[Double]], x_row: Rep[Int], x_col: Rep[Int], mu: Rep[Array[Double]], mu_row: Rep[Int], mu_col: Rep[Int]): Unit = {
          var dist = 0.0D
          var min_d = 0.0D
          var min_j = 0

          // if (!(x_row*x_col == x.length)) return;
          // if (!(mu_row * mu_col == mu.length)) return;
         var i = 0
         while(i < x_row) {
           min_d = -1.0D // TODO this makes issues
           min_j = -1
           var j = 0
           while(j < mu_row) {
             dist = 0.0D
             var k = 0
             while(k < x_col) {
               val tmp = x(i*x_col + k) - mu(j* mu_col + k)
               dist = dist + tmp * tmp
               k = k + 1
             }
             // HACK Why doesn't bytecode have || ???
             val lhs = min_d < -0.5
             val rhs = dist < min_d
             val cond = if (lhs == unit(false)) {
               if(rhs == unit(false)) {
                 unit(false)
               } else unit(true) // TODO elsless if
             } else unit(true)
             if(cond) {min_d = dist; min_j = j} else unit(())
             j = j + 1
           }
           v(i) = min_j
           i = i + 1
         }
  }
}

trait ComputCentroids extends DSL {
  def computeNewCentroids(v: Rep[Array[Int]], x: Rep[Array[Double]], x_row: Rep[Int], x_col: Rep[Int], mu: Rep[Array[Double]], mu_row: Rep[Int], mu_col: Rep[Int]): Unit = {

    // if (!(x_row*x_col == x.length)) return;
    // if (!(mu_row * mu_col == mu.length)) return;

    val weightedPoints = NewArray[Double](x_col)
    var i = 0
    while(i < mu_row) {
     var j = 0
     while(j < x_col) {weightedPoints(j) = 0.0D; j = j + 1}
       var points = 0
       j = 0
       while(j < x_row) {
         if(v(j) - i == 0) {
           var k = 0
           while(k < x_col) {
             weightedPoints(k) = weightedPoints(k) + x(j * x_col + k)
             k = k + 1
           }
           points = points + 1;
         }
         j = j + 1
       }
       if(points == 0) points = points + 1 else ()
       j = 0
       while(j < mu_col) {
         mu(i * mu_col + j) = weightedPoints(j) / points;
         j = j + 1
       }

      i= i + 1
    }
  }
}

class TestPerformance extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-performance"

  def testKMeans = withOutFile(prefix+"-kmeans") {

    withOutFile(prefix+"-kmeans") {
      trait Prog extends NearestCluster with ComputCentroids {
        def main(p: Rep[Int]): Rep[Int] = 1

        def main(x: Rep[Array[Double]], x_col: Rep[Int], mu: Rep[Array[Double]], mu_col: Rep[Int]): Rep[Array[Double]] = {
              val tol = 0.001D
              val x_row = x.length/x_col

              val mu_row = mu.length/mu_col

              val oldmu = NewArray[Double](mu_row * mu_col)

              var diff = tol + 1.0D
              val c = NewArray[Int](x_row)
              var iter = 0
              while (diff > tol) {
                iter = iter + 1
                array_copy(mu, 0, oldmu, 0, mu_row * mu_col)
                constructNearestClusterVector(c, x, x_row, x_col, mu, mu_row, mu_col)
                computeNewCentroids(c, x, x_row, x_col, mu, mu_row, mu_col)
                diff = 0.0D
                var i = 0
                while(i < mu_row) {
                  var j = 0
                  while(j < mu_col) {
                    diff = diff + Math.abs(mu(i * mu_col + j) - oldmu(i * mu_col + j))
                    j = j + 1
                  }
                  i = i + 1
                }
              }
              mu
        }
      }
      withOutFile(prefix+"-kmeans") {
        val f = (new Prog with KMeansImpl).function

        withOutFileChecked(prefix+"-kmeans-out") {
          println(f(Array.tabulate(9999)(i => scala.math.pow(0.963789D, i) + .23123D * i), 3, Array.tabulate(99)(i => i * 2.123123D), 3).mkString("centers[",",", "]"))
        }
      }
    }
  }
}
