/**
 * *****************************************************************************
 * Copyright (c) 2016-2017, KAIST.
 * All rights reserved.
 *
 * Use is subject to license terms.
 *
 * This distribution may include materials developed by third parties.
 * ****************************************************************************
 */
/**
 * Copyright (c) 2017, Oracle and/or its affiliates.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of KAIST, S-Core, Oracle nor the names of its contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This distribution may include materials developed by third parties.
 */

package kr.ac.kaist.safe.phase

import scala.util.{ Failure, Success, Try }
import kr.ac.kaist.safe.SafeConfig
import kr.ac.kaist.safe.analyzer._
import kr.ac.kaist.safe.analyzer.domain._
import kr.ac.kaist.safe.analyzer.domain.Utils._
import kr.ac.kaist.safe.nodes.cfg._
import kr.ac.kaist.safe.analyzer.models.builtin.BuiltinGlobal
import kr.ac.kaist.safe.nodes.ir.IRNode
import kr.ac.kaist.safe.util._

// Taint analysis prototype: detector phase
// MISSING: check non-primitive object arguments.
// MISSING: detecting property stores to sensitive (DOM) properties.

case object TaintDetect extends PhaseObj[(CFG, Int, TracePartition, Semantics), TaintDetectConfig, CFG] {
  val name: String = "taintDetector"
  val help: String = "Taint test."

  // Sink configuration
  val globalSinks = List("setInterval", "setTimeout", "eval")
  val mappedSinks = Map(
    "document" -> List("write"),
    "location" -> List("replace")
  )

  // Stores security-sensitive sinks we want to check
  private var taintSinks = Set[FunctionId]()

  // Bug report results
  case class TaintBug(node: IRNode, sink: FunctionId, args: List[Int])
  type BugList = List[TaintBug]

  private def isReachableUserCode(sem: Semantics, block: CFGBlock): Boolean =
    !sem.getState(block).isEmpty && !NodeUtil.isModeled(block)

  private def checkBlock(block: CFGBlock, semantics: Semantics): BugList =
    if (isReachableUserCode(semantics, block) && !block.getInsts.isEmpty) {
      val (_, st) = semantics.getState(block).head
      val (bugs, _) =
        block.getInsts.foldRight(List[TaintBug](), st)((inst, r) => {
          val (bs, state) = r
          inst match {
            // Check every call instruction
            case i: CFGCallInst =>
              // Target function(s) and argument object(s) for this call
              val funLocs = semantics.V(i.fun, state)._1.locset
              val argsLocs = semantics.V(i.arguments, state)._1.locset

              // Find tainted arguments
              val taintedArgs = argsLocs.foldLeft(List[Int]()) { (args, argsLoc) =>
                val argsObj = state.heap.get(argsLoc)
                // Determine length of argument object
                val lenVal = argsObj.Get("length", state.heap)
                val absLen = TypeConversionHelper.ToUInt32(lenVal)
                val len: Option[Int] = absLen.getSingle match {
                  case ConOne(n) => Option(n.toInt)
                  // Note: Ignore calls with imprecise argument count (unsound)
                  case _ => None
                }
                args ++ (0 until len.getOrElse(0)).filter { i =>
                  // Lookup argument value in argument object
                  val argVal = argsObj.Get(i.toString, state.heap)
                  // An argument that may be string-top may be tainted
                  argVal.pvalue.strval.isTop
                }
              }

              // Find calls to known sinks
              val calledSinks = funLocs.foldLeft(Set[FunctionId]()) { (cs, funloc) =>
                val o = state.heap.get(funloc)
                val fs = o(ICall).fidset
                cs ++ fs.filter(f => taintSinks(f))
              }

              // Construct bug reports
              val newBugs = if (taintedArgs.nonEmpty)
                calledSinks.toList.map { s =>
                  // Add a new bug (starting argument index at '1')
                  TaintBug(i.ir, s, taintedArgs map { i => i + 1 })
                }
              else
                List[TaintBug]()

              // Append newly found taint bugs
              (r._1 ++ newBugs, r._2)

            // Ignore all other instructions.
            case _ => r
          }
        })
      bugs
    } else List[TaintBug]()

  // Main functions of the TaintDetect phase
  def apply(
    in: (CFG, Int, TracePartition, Semantics),
    safeConfig: SafeConfig,
    config: TaintDetectConfig
  ): Try[CFG] = {
    val (cfg, _, tp, semantics) = in

    val st = semantics.getState(ControlPoint(cfg.globalFunc.exit, tp))
    val g: AbsObject = st.heap.get(BuiltinGlobal.loc)

    // Helper for heap lookups in model
    def getSingleLoc(absLoc: AbsLoc): AbsObject = absLoc.getSingle match {
      case ConOne(loc) => st.heap.get(loc)
      case _ => throw new AssertionError("expected single location")
    }

    // Helper: add sink function's internal ID to the taintSinks set
    def addSinkFunc(funcObj: AbsObject, name: String): Unit = {
      val fs = funcObj(ICall).fidset
      assert(fs.size == 1, "expected single location")
      val sink = fs.head

      // Report duplicates
      if (taintSinks.contains(sink))
        Console.err.println(s"Sink '$name' already exists")
      else
        taintSinks += sink
    }

    // Helper: lookup a sink function and add it the taintSinks set
    def addSink[T](objName: T, funcName: String): Unit = {
      val o: AbsObject = objName match {
        case l: Loc => st.heap.get(l)
        case s: String => getSingleLoc(g(s).value.locset)
      }

      val fo = getSingleLoc(o.Get(funcName, st.heap).locset)
      addSinkFunc(fo, funcName)
    }

    // Add sinks from configuration
    for (sinkName <- globalSinks) {
      addSink(BuiltinGlobal.loc, sinkName)
    }
    for ((objName, sinkList) <- mappedSinks) {
      val addSinkForObj = addSink(objName, _: String)
      for (f <- sinkList) {
        addSinkForObj(f)
      }
    }

    // Add sinks from a special global __sinks object in the user code, e.g.
    // __sinks = { "sink description": some_sink_function };
    // rhs value in __sinks must be a reference to a function object
    if (g.HasProperty(AbsString("__sinks"), st.heap) == AbsBool.True) {
      val o = getSingleLoc(g.Get("__sinks", st.heap).locset)
      o.abstractKeySet match {
        case ConFin(set) => set foreach { key =>
          val sinkName: String = key.getSingle match {
            case ConOne(s) => s
            case _ => throw new AssertionError("expected concrete __sink property key")
          }
          val fo = getSingleLoc(o.Get(key, st.heap).locset)
          addSinkFunc(fo, sinkName)
        }
        case ConInf() => throw new AssertionError("expected concrete __sink object")
      }
    }

    println(s"num sinks: ${taintSinks.size}")

    // Run taint detection
    val bugs = cfg.getUserBlocks.foldRight(List[TaintBug]())((b, r) => checkBlock(b, semantics) ++ r)

    // Print results
    if (bugs.nonEmpty) {
      println(s"=== ${bugs.size} taint bug(s) detected ===")
      bugs foreach { bug =>
        val sink = cfg.getFunc(bug.sink) match {
          case Some(func) => func.name
          case None => "<unknown>"
        }
        println(s"Call to sink '$sink' (tainted args: ${bug.args.mkString("{", ",", "}")}) @ ${bug.node.span}")
      }
      println("====")
    }

    // Done
    Success(cfg)
  }

  def defaultConfig: TaintDetectConfig = TaintDetectConfig()
  val options: List[PhaseOption[TaintDetectConfig]] = List(
    ("silent", BoolOption(c => c.silent = true),
      "messages during bug detection are muted.")
  )
}

// TaintDetect phase config
case class TaintDetectConfig(
  var silent: Boolean = false
) extends Config
