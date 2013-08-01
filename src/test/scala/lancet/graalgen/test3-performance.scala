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

class TestPerformance extends FileDiffSuite with GraalGenBase {

  val prefix = "test-out/test-graalgen-performance"

  def testInc = withOutFile(prefix+"-kmeans") {

    withOutFile(prefix+"-kmeans") {
      trait Prog extends DSL {
        def main(v: Rep[Int]): Rep[Int] = {

              val tol: Rep[Double] = 0.001

              val x = NewArray[Double](3);
              val x_row = x.length/3;
              val x_col = 3;//x[0].length;

              val mu = NewArray[Double](3);
              val mu_row = mu.length/3;
              val mu_col = 3;// mu[0]

              val oldmu = NewArray[Double](mu_row * mu_col);

              // System.out.println("Kmeans starting Computation");

              // long now = System.currentTimeMillis();
              var diff: Rep[Double] = tol + 1;
              val c = NewArray[Double](x_row);
              var iter = 0
              while (diff > tol) {
                iter = iter + 1;

                //System.arraycopy(mu, 0, oldmu, 0, mu_row * mu_col);

                // constructNearestClusterVector(c, x, x_row, x_col, mu, mu_row, mu_col)
                // computeNewCentroids(c, x, x_row, x_col, mu, mu_row, mu_col)

                diff = 0;
                var i = 0
                while(i < mu_row) {
                  var j = 0;
                  while(j < mu_col) {
                    // TODO diff += Math.abs(mu[i * mu_col + j] - oldmu[i *mu_col + j]);
                    j = j + 1
                  }
                  i = i + 1
                }
                diff = 0
              }


              // println("Elapsed time: "+ (System.currentTimeMillis() - now)/1000 + " seconds");
              // println("Finished in "+ iter + " iterations");

              // for(int i = 0; i < mu_row; i++) {
              //   for(int j = 0; j < mu_col; j++)
              //     System.out.print(mu[i * mu_col + j] + " ");
              //   System.out.println();
              // }
              v
        }
      }

      val f = (new Prog with Impl).function
      println(f(1))
    }
  }

}
