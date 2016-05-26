/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
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
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val v_fun = getArgValue(h, ctx, args, "1")
          val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))

          if (s_type </ StrBot && !v_fun.locs.isEmpty && b_capture </ BoolBot) {
            /* unsound, ingnore capture flag */
            val h_1 = DOMHelper.addEventHandler(h, s_type, v_fun, Value(lset_this))
            ((Helper.ReturnStore(h_1, Value(UndefTop)), ctx), (he, ctxe))
          } else {
            System.err.println("* Warning: addEventListener reached with incomplete arguments")
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      // do nothing : could be more precise
      ("EventTarget.removeEventListener" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
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
            val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
            /* arguments */
            val s_type = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
            val v_fun = getArgValue(h, ctx, args, "1")
            val b_capture = Helper.toBoolean(getArgValue(h, ctx, args, "2"))
            if (s_type </ StrBot && !v_fun.locs.isEmpty && b_capture </ BoolBot) {
              /* unsound, ingnore capture flag */
              val h_1 = DOMHelper.addEventHandler(h, s_type, v_fun, Value(lset_this))
              ((Helper.ReturnStore(h_1, Value(UndefTop)), ctx), (he, ctxe))
            } else {
              System.err.println("* Warning: addEventListener reached with incomplete arguments")
              ((HeapBot, ContextBot), (he, ctxe))
            }
          })),
      // do nothing : could be more precise
      ("window.EventTarget.removeEventListener" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
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
