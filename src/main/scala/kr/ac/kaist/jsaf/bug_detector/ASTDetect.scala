/*******************************************************************************
    Copyright (c) 2013-2014, KAIST.
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

package kr.ac.kaist.jsaf.bug_detector

import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.CallContext._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.{SemanticsExpr => SE}
import kr.ac.kaist.jsaf.compiler.Parser
import kr.ac.kaist.jsaf.nodes_util.{NodeRelation, Walkers}
import kr.ac.kaist.jsaf.scala_src.nodes._

class ASTDetect(bugDetector: BugDetector) {
  val ast                                       = bugDetector.ast
  val cfg                                       = bugDetector.cfg
  val bugStorage                                = bugDetector.bugStorage
  val stateManager                              = bugDetector.stateManager

  def check(): Unit = {
    object walker extends Walkers {
      override def walkAST(parent: Any, node: Any): Unit = {
        node match {
          // Check parsing arguments of "JSON.parse"
          case n@SFunApp(info, SDot(_, SVarRef(_, obj), mem), args)
            if obj.getText.equals("JSON") && mem.getText.equals("parse") && args.length > 0 =>
            NodeRelation.ast2cfgMap.get(n) match {
              case Some(cfgList) =>
                for (cfgInst <- cfgList) {
                  cfgInst match {
                    case inst@CFGCall(_, _, _, _, arguments, _, _) =>
                      val cfgNode = cfg.findEnclosingNode(inst)
                      val cstate = stateManager.getInputCState(cfgNode, inst.getInstId, _MOST_SENSITIVE)
                      for ((callContext, state) <- cstate) {
                        val argLocSet = SE.V(arguments, state.heap, state.context)._1.locs
                        for (argLoc <- argLocSet) {
                          val argObj = state.heap(argLoc)
                          if (argObj != null) {
                            // Check parsing the first argument of "JSON.parse"
                            val res = argObj("0")
                            if (res </ PropValueBot) {
                              val absStr = res.objval.value.pv.strval
                              absStr.gamma match {
                                case Some(vs) if !absStr.isAllNums =>
                                  val isNotBug = vs.exists(s => Parser.parseJSON(s))
                                  if(!isNotBug) bugStorage.addMessage(info.getSpan, ParseJSON, inst, callContext, absStr.toString())
                                case _ =>
                              }
                            }
                          }
                        }
                      }
                    case _ =>
                  }
                }
              case None =>
            }

          // Check parsing arguments of "new Function"
          case n@SNew(info, SFunApp(_, SVarRef(_, id), args)) if id.getText.equals("Function") && args.length > 0 =>
            NodeRelation.ast2cfgMap.get(n) match {
              case Some(cfgList) =>
                for (cfgInst <- cfgList) {
                  cfgInst match {
                    case inst@CFGConstruct(_, _, _, _, arguments, _, _) =>
                      val cfgNode = cfg.findEnclosingNode(inst)
                      val cstate = stateManager.getInputCState(cfgNode, inst.getInstId, _MOST_SENSITIVE)
                      for ((callContext, state) <- cstate) {
                        val argLocSet = SE.V(arguments, state.heap, state.context)._1.locs
                        for (argLoc <- argLocSet) {
                          val argObj = state.heap(argLoc)
                          if (argObj != null) {
                            // Check parsing arguments except the last of "new Function"
                            args.zipWithIndex.dropRight(1).foreach(p => {
                              val res = argObj(p._2.toString)
                              if (res </ PropValueBot) {
                                for(pvalue <- res.objval.value.pv) {
                                  if(pvalue.isConcrete) {
                                    var str = pvalue.toString
                                    if(str.startsWith("\"") && str.endsWith("\"")) str = str.substring(1, str.length-1)
                                    if(!Parser.parseFunctionParams(str))
                                      bugStorage.addMessage(info.getSpan, ParseFunctionParams, inst, callContext, str)
                                  }
                                }
                              }
                            })

                            // Check parsing the last argument of "new Function"
                            val res = argObj((args.length-1).toString)
                            if (res </ PropValueBot) {
                              for(pvalue <- res.objval.value.pv) {
                                if(pvalue.isConcrete) {
                                  var str = pvalue.toString
                                  if(str.startsWith("\"") && str.endsWith("\"")) str = str.substring(1, str.length - 1)
                                  if(!Parser.parseFunctionBody("{"+str+"}"))
                                    bugStorage.addMessage(info.getSpan, ParseFunctionBody, inst, callContext, str)
                                }
                              }
                            }
                          }
                        }
                      }
                    case _ =>
                  }
                }
              case None =>
            }
          case _ =>
        }

        // Walk child nodes
        super.walkAST(parent, node)
      }
    }

    // Walk AST nodes to collect only strict mode code ASTs
    walker.walkAST(null, ast)
  }
}
