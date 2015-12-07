/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing.models.DOMEvent

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models.DOMCore.DOMNode
import kr.ac.kaist.jsaf.analysis.typing.models.DOMObject.XMLHttpRequest
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.cfg.{CFG, CFGExpr, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.{ControlPoint, Helper, PreHelper}
import kr.ac.kaist.jsaf.analysis.typing.domain.Heap
import kr.ac.kaist.jsaf.analysis.typing.domain.Context
import kr.ac.kaist.jsaf.analysis.typing.models.AbsBuiltinFunc
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object EventTarget extends DOM {
  private val name = "EventTarget"

  /* predefined locatoins */
  val loc_proto = ObjProtoLoc
  val loc_ins = newSystemRecentLoc(name + "Ins")
  // no locations

  /* initial property list */
  override def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    // Node interface should implements EventTarget
    (DOMNode.loc_proto, prop_node_proto),
    // window interface should implements EventTarget
    (DOMWindow.WindowLoc, prop_window),
    // XMLHttpRequest
    (XMLHttpRequest.loc_proto, prop_ajax)
  )

  def getPropList = prop_node_proto
  /* prorotype */
  private val prop_node_proto: List[(String, AbsProperty)] = List(
    ("addEventListener",    AbsBuiltinFunc("EventTarget.addEventListener", 3)),
    ("removeEventListener", AbsBuiltinFunc("EventTarget.removeEventListener", 3)),
    ("dispatchEvent",       AbsBuiltinFunc("EventTarget.dispatchEvent", 1))
  )
  private val prop_window: List[(String, AbsProperty)] = List(
    ("addEventListener",    AbsBuiltinFunc("window.EventTarget.addEventListener", 3)),
    ("removeEventListener", AbsBuiltinFunc("window.EventTarget.removeEventListener", 3)),
    ("dispatchEvent",       AbsBuiltinFunc("window.EventTarget.dispatchEvent", 1))
  )
  
  private val prop_ajax: List[(String, AbsProperty)] = List(
    ("addEventListener",    AbsBuiltinFunc("window.EventTarget.addEventListener", 3)),
    ("removeEventListener", AbsBuiltinFunc("window.EventTarget.removeEventListener", 3)),
    ("dispatchEvent",       AbsBuiltinFunc("window.EventTarget.dispatchEvent", 1))
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("EventTarget.addEventListener" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          /* arguments */
          val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val v_fun = getArgValue(h, ctx, args, "1")
          val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))
          if (s_type </ StrBot && v_fun </ ValueBot && b_capture </ BoolBot) {
            /* unsound, ingnore capture flag */
            val h_1 = DOMHelper.addEventHandler(h, s_type, v_fun, Value(lset_this))
            ((Helper.ReturnStore(h_1, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      // do nothing : could be more precise
      ("EventTarget.removeEventListener" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          /* arguments */
          val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val v_fun = getArgValue(h, ctx, args, "1")
          val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))
          if (s_type </ StrBot) {
            ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),

        // case "EventTarget.dispatchEvent" =>


        ("window.EventTarget.addEventListener" -> (
          (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
            val lset_this = h(SinglePureLocalLoc)("@this")._2._2
            /* arguments */
            val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
            val v_fun = getArgValue(h, ctx, args, "1")
            val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))
            if (s_type </ StrBot && v_fun </ ValueBot && b_capture </ BoolBot) {
              /* unsound, ingnore capture flag */
              val h_1 = DOMHelper.addEventHandler(h, s_type, v_fun, Value(lset_this))
              ((Helper.ReturnStore(h_1, Value(UndefTop)), ctx), (he, ctxe))
            }
            else
              ((HeapBot, ContextBot), (he, ctxe))
          })),
      // do nothing : could be more precise
      ("window.EventTarget.removeEventListener" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          /* arguments */
          val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val v_fun = getArgValue(h, ctx, args, "1")
          val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))
          if (s_type </ StrBot) {
            ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }))
      // case "window.EventTarget.dispatchEvent" =>
    )
  }


  /* instance */
  //def instantiate() = Unit // not yet implemented
  // intance of EventTarget should have 'name', 'sepcified', 'value', 'ownerElement', 'schemaTypeInfo', 'isId' property
}
