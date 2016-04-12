/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing

import scala.collection.immutable.{ TreeMap, TreeSet, HashSet, HashMap, Stack => IStack }
import scala.collection.mutable.{ HashMap => MHashMap, HashSet => MHashSet, Stack => MStack }
import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.lib.graph.DGraph
import kr.ac.kaist.jsaf.analysis.lib.WorkTreeSet
import kr.ac.kaist.jsaf.{Shell, ShellParameters}
import kr.ac.kaist.jsaf.scala_src.useful.WorkTrait

/**
 * Worklist manager
 */
abstract class Worklist {
  ////////////////////////////////////////////////////////////////////////////////
  // Variables
  ////////////////////////////////////////////////////////////////////////////////
  private var cfg: CFG = null
  private var order: OrderMap = null
  private var headorder: OrderMap = TreeMap[Node, Int]()
  private var backedges: HashMap[Node, HashSet[Node]] = HashMap()
  private var quiet: Boolean = false

  ////////////////////////////////////////////////////////////////////////////////
  // Abstract Functions
  ////////////////////////////////////////////////////////////////////////////////
  def head: ControlPoint
  def isEmpty: Boolean
  def getSize: Int
  def getWorkList: WorkTreeSet
  def toString: String
  protected def insertWork(work: OrderEntry): Unit
  protected def removeHead: ControlPoint

  ////////////////////////////////////////////////////////////////////////////////
  // Initialization
  ////////////////////////////////////////////////////////////////////////////////
  // Dense init
  def init(cfg: CFG, order: OrderMap, quiet: Boolean): Unit = {
    this.cfg = cfg
    this.order = order
    this.quiet = quiet
  }

  // Sparse init
  def init(cfg: CFG, order: OrderMap, quiet: Boolean, headorder: OrderMap, backedges: HashMap[Node, HashSet[Node]]): Unit = {
    init(cfg, order, quiet)
    this.headorder = headorder
    this.backedges = backedges
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Add a Work
  ////////////////////////////////////////////////////////////////////////////////
  def add(cp: ControlPoint): Unit = {
    val ov = if(order == null) 0 else order.getOrElse(cp._1, 0) // 0 => case for an empty block
    insertWork((ov, cp))
    if(useWorkManager) Shell.workManager.pushWork(workTrait)
  }

//  TODO UNUSED CODE
//  def add(origin: Node, cp: ControlPoint): Unit = {
//    backedges.get(cp._1) match {
//      case Some(backnodes) if backnodes.contains(origin) =>
//        insertWork((headorder(cp._1), cp))
//        if(useWorkManager) Shell.workManager.pushWork(workTrait)
//      case Some(backnodes) => add(cp)
//      case _ => add(cp)
//    }
//  }

  ////////////////////////////////////////////////////////////////////////////////
  // etc.
  ////////////////////////////////////////////////////////////////////////////////
  def getOrder(): OrderMap = order
  def getHead(): ControlPoint = removeHead

  class PrettyPrinter {
    var Seen = Set[FunctionId]()
    def apply(cp: ControlPoint): String = {
      val (funId, funName) = (cp._1._1, cfg.getFuncName(cp._1._1))
      val funPretty = cfg.getFuncDisplayName(funId).getOrElse(funName)
      val funShort = if (funPretty.length < 15) {
        "%d:'%s'".format(funId, funPretty)
      } else {
        if (!Seen.contains(funId)) {
          System.out.println("Hint: function ID %d -> %s".format(funId, funName))
          Seen += funId
        }
        funId.toString
      }
      val labelName = cp._1._2 match {
        case LBlock(id) => id.toString
        case LEntry => "Entry"
        case LExit => "Exit"
        case LExitExc => "Exc"
      }
      "%s()::%s".format(funShort, labelName) + " ctxt=[" + cp._2.toString + "]"
    }
  }
  val prettyPrint = new PrettyPrinter

  def debugNext: String = prettyPrint(head)

  ////////////////////////////////////////////////////////////////////////////////
  // For WorkManager (Thread library)
  ////////////////////////////////////////////////////////////////////////////////
  private var useWorkManager = false
  private var workTrait: WorkTrait = null
  def setUseWorkManager(_useWorkManager: Boolean, _workTrait: WorkTrait): Unit = {
    useWorkManager = _useWorkManager
    workTrait = _workTrait
  }
}

object Worklist {
  ////////////////////////////////////////////////////////////////////////////////
  // Worklist Order Types
  ////////////////////////////////////////////////////////////////////////////////
  final val WORKLIST_ORDER_DEFAULT: Int = 0
  final val WORKLIST_ORDER_FIFO: Int = 1
  final val WORKLIST_ORDER_LIFO: Int = 2
  final val WORKLIST_ORDER_COUNT: Int = 3

  ////////////////////////////////////////////////////////////////////////////////
  // Worklist Computes
  ////////////////////////////////////////////////////////////////////////////////
  def computes(cfg: CFG) : Worklist = 
    if(Shell.params.command == ShellParameters.CMD_WEBAPP_BUG_DETECTOR)
      computes(Shell.params.opt_WorklistOrder, cfg, true)
    else
      computes(Shell.params.opt_WorklistOrder, cfg, false)
  def computes(cfg: CFG, quiet: Boolean) : Worklist = computes(Shell.params.opt_WorklistOrder, cfg, quiet)
  def computes(orderType: Int, cfg: CFG, quiet: Boolean) : Worklist = {
    val startTime = System.nanoTime
    var worklist: Worklist = null
    orderType match {
      case WORKLIST_ORDER_DEFAULT =>
        val empty = TreeMap[Node, Int]()
        val (map, _) = cfg.getNodes.foldLeft((empty, 0))((m, n) => (m._1 + (n -> m._2), m._2 + 1))
        worklist = new WorklistDefault
        worklist.init(cfg, map, quiet)
      case WORKLIST_ORDER_FIFO =>
        worklist = new WorklistFIFO
        worklist.init(cfg, null, quiet)
      case WORKLIST_ORDER_LIFO =>
        worklist = new WorklistLIFO
        worklist.init(cfg, null, quiet)
      case WORKLIST_ORDER_COUNT =>
        worklist = new WorklistCount
        worklist.init(cfg, null, quiet)
    }
    if (!quiet) {
      val elapsedTime = (System.nanoTime - startTime) / 1000000000.0
      System.out.format("# Time for worklist order computation(s): %.2f\n", new java.lang.Double(elapsedTime))
    }
    worklist
  }

}
