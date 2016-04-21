package kr.ac.kaist.jsaf.analysis.imprecision

import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._

import scala.collection.mutable.Queue

class ImprecisionStop(msg: String) extends RuntimeException(msg)

object ImprecisionTracker {
  private val stopEnabled = true
  private var wlSize = 0

  // Helpers
  private def log(x: Any) = System.out.println(x)
  private def stopFixpoint(msg: String) = {
    // XXX uses an existing exception to bail out of fixpoint analysis
    if (stopEnabled)
      throw new ImprecisionStop(s"Imprecision: $msg")
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
      println("AI back-buffer:")
      for (x <- ringbuffer) {
        val insts = x.block.insts
        log(s"${x.index}: block[${insts.size}]=${insts.toString}")
      }
    }
  }

  private def handleLoadStoreProp(propSet: Set[AbsString], msg: String, exceptionMsg: String): Unit = {
    val nonConcrete = propSet filter (s => s.getSingle.isEmpty)
    nonConcrete foreach { s =>
      log(msg.format(s))
    }
    if (nonConcrete.nonEmpty)
      stopFixpoint(exceptionMsg)
  }

  def propStore(propSet: Set[AbsString], propVal: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"lossy store[%s] @ ${info.getSpan}", "non-concrete store")
  }

  def propLoad(propSet: Set[AbsString], v: Value, info: Info) = {
    handleLoadStoreProp(propSet, s"lossy load[%s] @ ${info.getSpan}", "non-concrete load")
  }

  def callViaLocations(funLocs: LocSet, info: Info) = {
    if (funLocs.size > 1) {
      log(s"Target function set size = ${funLocs.size} @ ${info.getSpan}")
      stopFixpoint("imprecise call")
    }
  }

  def random(msg: String): Unit = {
    log(s"Use of imprecise random function: $msg")
    stopFixpoint("random() function")
  }

  def updateWorklistSize(size: Int) = {
    if (size - wlSize > 25) {
      log(s"WL increase $wlSize -> $size")
      BackBuffer.dump
      stopFixpoint("worklist explosion")
    }
    wlSize = size
  }

  def updateBlock(iteration: Int, cmd: Cmd): Unit = {
    cmd match {
      case b: Block => BackBuffer.enqueue(iteration, b)
      case _ => ()
    }
    BackBuffer.trim
  }
}
