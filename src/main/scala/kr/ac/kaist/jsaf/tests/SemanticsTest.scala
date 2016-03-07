/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/
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

package kr.ac.kaist.jsaf.tests

import java.io.File
import java.io.OutputStream
import java.io.PrintStream

import kr.ac.kaist.jsaf.exceptions.UserError

import scala.collection.immutable.HashSet
import scala.collection.mutable
import junit.framework.Assert.fail
import junit.framework.TestCase
import kr.ac.kaist.jsaf.analysis.cfg.CFG
import kr.ac.kaist.jsaf.analysis.cfg.CFGBuilder
import kr.ac.kaist.jsaf.analysis.cfg.LExit
import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.models.{BuiltinModel, ModelManager}
import kr.ac.kaist.jsaf.compiler.Disambiguator
import kr.ac.kaist.jsaf.compiler.Hoister
import kr.ac.kaist.jsaf.compiler.Parser
import kr.ac.kaist.jsaf.compiler.Translator
import kr.ac.kaist.jsaf.compiler.WithRewriter
import kr.ac.kaist.jsaf.nodes.IRRoot
import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.nodes_util.NodeRelation
import kr.ac.kaist.jsaf.scala_src.useful.Options._

class SemanticsTest(dir: String, tc: String, typing_mode: String) extends TestCase(tc) {
  // "pre", "dense", "sparse", "dsparse"

  // "no", "1callsite", "1obj", "1tajs"
  val CONTEXT_SENSITIVITY = "1callsite" 

  private class NullOutputStream extends OutputStream {
    override def write(x: Int) = () 
  }

  override def runTest() = {
    // silence stdout, stderr
    val oldOut = System.out
    val oldErr = System.err
    val nullStream = new PrintStream(new NullOutputStream())
    System.setErr(nullStream)
    System.setOut(nullStream)

    try {
      PreConfig.strings = TestStringConfig(StrDomainSAFE)
      // Initialize AddressManager
      AddressManager.reset()
      val typing = analyze(new File(dir, tc))
      SemanticsTest.checkResult(typing);
    } finally {
      // recover stdout, stderr
      System.setErr(oldErr)
      System.setOut(oldOut)
    }
  }
  
  def analyze(file: File): TypingInterface = {
    // setup testing options
    Config.setTestMode(true)
    Config.setAssertMode(true)

    // parse
    var program: Program = Parser.parseFileConvertExn(file)

    // hoist
    val hoister = new Hoister(program);
    program = hoister.doit().asInstanceOf[Program]

    // disambiguate
    val disambiguator = new Disambiguator(program, false)
    program = disambiguator.doit().asInstanceOf[Program];

    // with rewrite
    val withRewriter = new WithRewriter(program, false);
    program = withRewriter.doit().asInstanceOf[Program]

    // translate to IR
    val translator = new Translator(program, toJavaOption(None));
    val ir: IRRoot = translator.doit().asInstanceOf[IRRoot];

    // build CFG
    val builder = new CFGBuilder(ir);
    val cfg: CFG = builder.build();

    NodeRelation.set(program, ir, cfg, true)

    // initialize heap
    //val model = new BuiltinModel(cfg)
    //model.initialize()
    val init = new InitHeap(cfg)
    init.initialize()

    // typing
    val typing: TypingInterface =
      typing_mode match {
        case "dense"   => new Typing(cfg, false, false)
        case _ => throw new UserError("not supported")
      }
    
    CONTEXT_SENSITIVITY match {
      case "no"        => Config.setContextSensitivityMode(Config.Context_Insensitive)
      case "1callsite" => Config.setContextSensitivityMode(Config.Context_OneCallsite)
      case "1obj"      => Config.setContextSensitivityMode(Config.Context_OneObject)
      case "1tajs"     => Config.setContextSensitivityMode(Config.Context_OneObjectTAJS)
    }
    
    Config.setTypingInterface(typing)
    Config.setAssertMode(true)
    //typing.analyze(init_heap)

    typing_mode match {
      case  "dense" =>
        typing.analyze(init)
      case "pre" | "sparse" | "dsparse" =>
        throw new UserError("Not supported")
    }

    // return resulting Typing instance
    typing
  }
}

object SemanticsTest {
  val RESULT = "__result"
  val EXPECT = "__expect"
  val EXPECTEQ = "___expect"  // enforce equality (===) check

  def checkResult(typing: TypingInterface) = {
    // find global object at program exit node
    val obj = Helper.getHeapAtExit(typing)(GlobalLoc)

    val map = try {
      obj.getProps
    } catch {
      case _: Throwable =>
        fail("Global object is not found at program exit node")
        HashSet()
    }

    // collect result/expect values
    val resultMap = mutable.Map[Int, Value]()
    val expectMap = mutable.Map[Int, (Value, Boolean)]()

    for (prop <- map) {
      try {
        val pvalue = obj(prop)
        def index(keyword: String) = prop.substring(keyword.length).toInt
        if (prop.startsWith(RESULT)) {
          resultMap += (index(RESULT) -> pvalue._1._1)
        } else if (prop.startsWith(EXPECT)) {
          expectMap += (index(EXPECT) -> (pvalue._1._1, false))
        } else if (prop.startsWith(EXPECTEQ)) {
          // ___expect will enforce equality (===) between the (abstract) values
          expectMap += (index(EXPECTEQ) -> (pvalue._1._1, true))
        }
      } catch {
        case _: Throwable => fail("Invalid result/expect variable found: " + prop.toString)
      }
    }

    // invalid number of result/expect entries
    if (resultMap.size == 0) {
        fail("map.size: "+map.size+", resultMap : " +resultMap.toString)
    //  fail("No result/expect variable is detected " + map.size)
    }
    if (resultMap.size != expectMap.size)
      fail("Unmatched result/expect variable")

    // check expect <= result
    for ((index, result) <- resultMap.toSeq.sortBy(_._1)) {
      expectMap.get(index) match {
        case None =>
          fail("No corresponding expect variable is detected for " +
               RESULT +
               index.toString)
        case Some((expect, testEq)) =>
          val success = expect <= result && (!testEq || result <= expect)
          if (!success) {
            val sb = new StringBuilder
            sb.append(RESULT)
            sb.append(index.toString)
            sb.append(" = {")
            sb.append(DomainPrinter.printValue(result))
            if (testEq)
              sb.append("} === {")
            else
              sb.append("} >= {")
            sb.append(DomainPrinter.printValue(expect))
            sb.append("}")
            fail(sb.toString)
          }
      }
    }
  }
}
