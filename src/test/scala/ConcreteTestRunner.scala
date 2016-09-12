/*******************************************************************************
 Copyright (c) 2016, Oracle and/or its affiliates.
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of KAIST, S-Core, Oracle nor the names of its contributors
   may be used to endorse or promote products derived from this software without
   specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 This distribution may include materials developed by third parties.
 ******************************************************************************/

import java.io.File

import junit.framework.{TestSuite, Test}
import junit.framework.TestCase
import kr.ac.kaist.jsaf.compiler.Predefined
import kr.ac.kaist.jsaf.tests.ConcreteECMASemantics
import kr.ac.kaist.jsaf.{Shell, ShellParameters}


class ConcreteTestSuite extends TestCase("Concrete") {}

object ConcreteTestSuite {
  val TESTS_DIR = "tests/concrete_semantics/ecma"

  Shell.pred = new Predefined(new ShellParameters())

  def suite: Test = {
    val suite = new TestSuite("ConcreteSuite")

    def collect(path: File): List[File] = {
      var l = List[File]()
      // TODO could apply filter here
      Option(path.listFiles()) match {
        case Some(files) =>
          files.foreach(f =>
            if (f.isDirectory)
              l ++= collect(f)
            else
              l :+= f
          )
        case None => ()
      }
      l
    }


    // TODO filter for files is .js extension
    for (file <- collect(new File(TESTS_DIR)).filter(f => f.getName.endsWith(".js")))
      suite.addTest(new ConcreteECMASemantics(file))
    suite

  }
}
