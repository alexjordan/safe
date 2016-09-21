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

package kr.ac.kaist.jsaf.analysis.imprecision

import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml.HTMLCollection

import scala.collection.mutable.Queue

class ImprecisionStop(msg: String) extends RuntimeException(msg)

sealed abstract class ImprecisionHint {
  def isDefined = true
  var linked = Set[ImprecisionHint]()
  def +(rhs: ImprecisionHint): ImprecisionHint = {
    rhs match {
      case NoHint => this
      case _ => { rhs.linked += this; rhs }
    }
  }
  def linkedMsg = if (linked.nonEmpty) s" {+ ${linked.size} more}" else ""
}

object NoHint extends ImprecisionHint {
  override def isDefined = false
  override def +(rhs: ImprecisionHint): ImprecisionHint = rhs
}


case class JoinHint(msg: String, inst: CFGInst) extends ImprecisionHint {
  override def toString = s"$msg @ " + (inst.getInfo match {
      case Some(info) => info.getSpan
      case None => "unknown"
    }) + linkedMsg
}

case class ModelHint(msg: String, src: StackTraceElement) extends ImprecisionHint {
  override def toString = s"$msg (source: ${src.getFileName}:${src.getLineNumber})"
}

case class RandomHint(msg: String) extends ImprecisionHint {
  override def toString = s"random: $msg"
}

object ImprecisionTracker {

  var stopEnabled = false
  private var wlSize = 0
  private var domTracking: DOMLookup = EmptyDOMLookup

  sealed abstract class DOMLookup {
    def handleReturn(heap: Heap, ret: PropValue): Option[ImprecisionHint] = None
  }

  object EmptyDOMLookup extends DOMLookup

  class DOMLookupByKey(val key: AbsString, val src: StackTraceElement) extends DOMLookup {
    override def handleReturn(heap: Heap, ret: PropValue): Option[ImprecisionHint] = {
      def createHint = {
        val lookupLoc = Instruction match {
          case Some(inst) => inst.getInfo match {
            case Some(info) => s" @ ${info.getSpan}"
            case None => ""
          }
          case None => ""
        }
        log(s"DOM lookup loss: ${key}")
        ModelHint(s"DOM lookup loss: ${key}${lookupLoc}", src)
      }

      // definite null
      if (ret._2 == Value(NullTop))
        return None

      // check for over-/under-approximated result
      val locs = ret._1._1.locs
      if (locs.size == 1) {
        val o = heap(locs.head)
        val protoval = o("@proto")._2
        assert(protoval.locs.size == 1)
        protoval.locs.head match {
          case HTMLCollection.loc_proto =>
            if (o("@default_number") </ PropValueBot)
              // non-deterministic collection
              return Some(createHint)
          case ObjProtoLoc => // single object being returned
          case _ => throw new InternalError(s"unexpted DOM return proto: ${DomainPrinter.printValue(protoval)}")
        }
      } else if (locs.isEmpty || locs.size > 1) {
        // no or more than one object being returned
        return Some(createHint)
      }

      None
    }
  }


  var Iteration: Int = -1
  var Instruction: Option[CFGInst] = None
  var InstHint: ImprecisionHint = NoHint

  // Helpers
  private var log = (x: Any) => () // logging disabled by default
  def disableLog { log = (x: Any) => () }
  def enableLog { log = (x: Any) => System.out.println(x) }
  private def prefix(kind: String): String = s"lossy-$kind[i=$Iteration]"

  private def stopFixpoint(msg: String) = {
    if (stopEnabled)
      throw new ImprecisionStop(s"Imprecision Exception: $msg")
  }

  private object BackBuffer {

    case class Payload(index: Int, block: Block)

    type BufferContents = Payload
    private var ringbuffer = Queue[BufferContents]()

    def trim = {
      // enforce max ring buffer size
      ringbuffer = ringbuffer.takeRight(5)
    }

    def enqueue(i: Int, b: Block) = ringbuffer.enqueue(Payload(i, b))

    def dump: Unit = {
      println("--- AI back-buffer:")
      for (x <- ringbuffer) {
        val insts = x.block.insts
        log(s"${x.index}: block[${insts.size}]=${insts.toString}")
      }
      println("---")
    }
  }

  private def handleLoadStoreProp(propSet: Set[AbsString], msg: String, exceptionMsg: String,
                                  imphint: ImprecisionHint): Unit = {
    val nonConcrete = propSet filter (s => s.getSingle.isEmpty)
    nonConcrete foreach { s =>
      log(msg.format(s))
    }
    if (nonConcrete.nonEmpty) {
      report("non-concrete propaccess", imphint)
      stopFixpoint(exceptionMsg)
    } else if (imphint.isDefined) {
      report("concrete propaccess", imphint)
    }
  }

  def propStore(propSet: Set[AbsString], propVal: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"${prefix("propaccess")} store[%s] @ ${info.getSpan}", "non-concrete store",
      propVal.imphint)
  }

  def propLoad(propSet: Set[AbsString], propVal: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"${prefix("propaccess")} load[%s] @ ${info.getSpan}", "non-concrete load",
      propVal.imphint)
  }

  def callViaLocations(v: Value, funLocs: LocSet, info: Info) = {
    report(s"call (${funLocs.size} @ ${info.getSpan})", v.imphint)
    if (funLocs.size > 1) {
      log(s"Target function set size = ${funLocs.size} @ ${info.getSpan}")
      stopFixpoint("imprecise call")
    }
  }

  def applyViaLocations(v: Value, funLocs: LocSet, fun: String) = {
    report(s"apply (${funLocs.size} @ $fun)", v.imphint)
    if (funLocs.size > 1) {
      stopFixpoint("imprecise apply")
    }
  }

  def impreciseModel(model: String): Unit = {
    val st = Thread.currentThread().getStackTrace
    InstHint += ModelHint(model, st(2))
  }

  def random(msg: String): Unit = {
    InstHint += RandomHint(msg)
  }

  def updateWorklistSize(size: Int) = {
    if (size - wlSize > 25) {
      log(s"WL increase $wlSize -> $size")
      BackBuffer.dump
      stopFixpoint("worklist explosion")
    }
    wlSize = size
  }

  @deprecated
  def nextBlock(cmd: Cmd): Unit = {
    cmd match {
      case b: Block => BackBuffer.enqueue(Iteration, b)
      case _ => ()
    }
    BackBuffer.trim
  }

  def joinLoss(lhs: AbsString, rhs: AbsString, res: AbsString) = {
    InstHint += JoinHint(s"str-join $lhs + $rhs -> $res", Instruction.get)

    val span = Instruction.get.getInfo match {
      case Some(info) => info.getSpan
      case None => "unknown"
    }
    //log(s"${prefix("join")}: $lhs + $rhs -> $res @ $span")
  }

  def joinLoss(lhs: AbsNumber, rhs: AbsNumber, res: AbsNumber) = {
    InstHint += JoinHint(s"num-join $lhs + $rhs -> $res", Instruction.get)

    val span = Instruction.get.getInfo match {
      case Some(info) => info.getSpan
      case None => "unknown"
    }
    log(s"${prefix("num-join")}: $lhs + $rhs -> $res @ $span")
  }

  def trackDOMLookup(id: AbsString, result: LocSet) = {
    val st = Thread.currentThread().getStackTrace
    //if (id.isConcrete)
      domTracking = new DOMLookupByKey(id, st(2))
  }

  def DOMReturn(heap: Heap): Heap = {
    val ret: PropValue = heap(SinglePureLocalLoc)("@return")
    val impHint = domTracking.handleReturn(heap, ret)
    if (impHint.nonEmpty) {
      Value.hint(ret._2, impHint.get)
      //return heap.update(SinglePureLocalLoc, heap(SinglePureLocalLoc).update("@return", hinted))
    }
    heap
  }

  def nextIteration(i: Int): Unit = {
    Iteration = i
  }

  def nextInstruction(inst: CFGInst) = {
    Instruction = Some(inst)
    domTracking = EmptyDOMLookup
    InstHint = NoHint
  }

  def report(info: String, imphint: ImprecisionHint): Unit = {
    imphint match {
      case NoHint => ()
      case h: ImprecisionHint => log(s"Imprecision reaches '$info': $h")
    }
  }

  def valueLocsAccess(hint: ImprecisionHint): Unit = {
    InstHint += hint
    //log(s"Imprecise locs access [$Iteration] @ $Instruction: $hint")
  }

  def valuePVAccess(hint: ImprecisionHint): Unit = {
    InstHint += hint
    //log(s"Imprecise pv access [$Iteration] @ $Instruction: $hint")
  }
}
