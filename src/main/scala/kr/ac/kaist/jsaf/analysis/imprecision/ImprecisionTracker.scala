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

import scala.collection.mutable.Queue

class ImprecisionStop(msg: String) extends RuntimeException(msg)

object ImprecisionTracker {

  // TODO these should be configurable
  private val MAX_WLSIZE_INCREASE = 100
  private val MAX_FUNLOCS = 20

  var stopEnabled = true
  private var wlSize = 0

  var Iteration: Int = -1
  var Instruction: Option[CFGInst] = None

  // Helpers
  private var log = (x: Any) => () // logging disabled by default
  private def prefix(kind: String): String = s"lossy-$kind[i=$Iteration]"

  private def stopFixpoint(msg: String) = {
    if (stopEnabled)
      throw new ImprecisionStop(s"Imprecision Exception: $msg")
  }

  private def handleLoadStoreProp(propSet: Set[AbsString], msg: String, exceptionMsg: String): Unit = {
    val nonConcrete = propSet filter (s => s.getSingle.isEmpty)
    nonConcrete foreach { s =>
      log(msg.format(s))
    }
  }

  def disableLog { log = (x: Any) => () }
  def enableLog { log = (x: Any) => System.out.println(x) }


  def callViaLocations(funLocs: LocSet, info: Info) = {
    if (funLocs.size > MAX_FUNLOCS) {
      log(s"Target function set size = ${funLocs.size} @ ${info.getSpan}")
      stopFixpoint(s"imprecise call (${funLocs.size} targets)")
    }
  }

  def propStore(propSet: Set[AbsString], propVal: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"${prefix("propaccess")} store[%s] @ ${info.getSpan}", "non-concrete store")
  }

  def propLoad(propSet: Set[AbsString], v: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"${prefix("propaccess")} load[%s] @ ${info.getSpan}", "non-concrete load")
  }

  def updateWorklistSize(size: Int) = {
    if (size - wlSize > MAX_WLSIZE_INCREASE) {
      log(s"WL increase $wlSize -> $size")
      stopFixpoint("worklist explosion")
    }
    wlSize = size
  }

  def joinLoss(lhs: AbsString, rhs: AbsString, res: AbsString): Unit = {
    // join outside an actual instruction, probably running tests
    if (Instruction.isEmpty)
      return

    val span = Instruction.get.getInfo match {
      case Some(info) => info.getSpan
      case None => "unknown"
    }

    log(s"${prefix("join")}: $lhs + $rhs -> $res @ $span")
  }

  def nextIteration(i: Int): Unit = {
    Iteration = i
  }

  def nextInstruction(inst: CFGInst) = {
    Instruction = Some(inst)
  }

}
