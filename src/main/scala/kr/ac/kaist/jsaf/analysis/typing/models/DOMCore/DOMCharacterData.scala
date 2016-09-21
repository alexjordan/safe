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

package kr.ac.kaist.jsaf.analysis.typing.models.DOMCore

import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T, _}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.cfg._
import org.w3c.dom.Node
import org.w3c.dom.CharacterData
import kr.ac.kaist.jsaf.analysis.typing.models.AbsConstValue
import kr.ac.kaist.jsaf.analysis.typing.domain.Heap
import kr.ac.kaist.jsaf.analysis.typing.domain.Context
import kr.ac.kaist.jsaf.analysis.typing.models.AbsBuiltinFunc
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object DOMCharacterData extends DOM {
  private val name = "CharacterData"

  /* predefined locatoins */
  val loc_cons = newSystemRecentLoc(name + "Cons")
  val loc_proto = newSystemRecentLoc(name + "Proto")
  val loc_ins = newSystemRecentLoc(name + "Ins")

  /* constructor */
  private val prop_cons: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("@hasinstance", AbsConstValue(PropValueNullTop)),
    ("length", AbsConstValue(PropValue(ObjectValue(Value(AbsNumber.alpha(0)), F, F, F)))),
    ("prototype", AbsConstValue(PropValue(ObjectValue(Value(loc_proto), F, F, F))))
  )

  /* prorotype */
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(DOMNode.loc_proto), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("substringData", AbsBuiltinFunc("DOMCharacterData.substringData", 2)),
    ("appendData",    AbsBuiltinFunc("DOMCharacterData.appendData", 1)),
    ("insertData",    AbsBuiltinFunc("DOMCharacterData.insertData", 2)),
    ("deleteData",    AbsBuiltinFunc("DOMCharacterData.deleteData", 2)),
    ("replaceData",   AbsBuiltinFunc("DOMCharacterData.replaceData", 3))
  )

  /* global */
  private val prop_global: List[(String, AbsProperty)] = List(
    (name, AbsConstValue(PropValue(ObjectValue(loc_cons, T, F, T))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("DOMCharacterData.substringData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val n_offset = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val n_count  = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (n_offset </ NumBot || n_count </ NumBot)
          /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMCharacterData.appendData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val s_arg = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (s_arg </ StrBot) {
            /* imprecise semantic */
            val h1 = lset_this.foldLeft(h)((hh, l) => {
              val hhh = Helper.PropStore(hh, l, AbsString.alpha("data"), Value(StrTop))
              Helper.PropStore(hhh, l, AbsString.alpha("length"), Value(UInt))
            })
            ((Helper.ReturnStore(h1, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMCharacterData.insertData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val n_offset = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val s_arg    = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (s_arg </ StrBot || n_offset </ NumBot) {
            /* imprecise semantic */
            val h1 = lset_this.foldLeft(h)((hh, l) => {
              val hhh = Helper.PropStore(hh, l, AbsString.alpha("data"), Value(StrTop))
              Helper.PropStore(hhh, l, AbsString.alpha("length"), Value(UInt))
            })
            ((Helper.ReturnStore(h1, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMCharacterData.deleteData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val n_offset = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val n_count  = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (n_offset </ NumBot || n_count </ NumBot) {
            /* imprecise semantic */
            val h1 = lset_this.foldLeft(h)((hh, l) => {
              val hhh = Helper.PropStore(hh, l, AbsString.alpha("data"), Value(StrTop))
              Helper.PropStore(hhh, l, AbsString.alpha("length"), Value(UInt))
            })
            ((Helper.ReturnStore(h1, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMCharacterData.replaceData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val n_offset = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val n_count  = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          val s_arg    = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "2")))
          if (n_offset </ NumBot || n_count </ NumBot || s_arg </ StrBot) {
            /* imprecise semantic */
            val h1 = lset_this.foldLeft(h)((hh, l) => {
              val hhh = Helper.PropStore(hh, l, AbsString.alpha("data"), Value(StrTop))
              Helper.PropStore(hhh, l, AbsString.alpha("length"), Value(UInt))
            })
            ((Helper.ReturnStore(h1, Value(UndefTop)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }))
    )
  }

  /* instance */
  override def getInstance(cfg: CFG): Option[Loc] = Some(newRecentLoc())
  /* list of properties in the instance object */
  override def getInsList(node: Node): List[(String, PropValue)] = node match {
    case c: CharacterData =>
      // This instance object has all properties of the Node object
      DOMNode.getInsList(node) ++ List(
        // DOM Level 1
        ("data",   PropValue(ObjectValue(AbsString.alpha(c.getData), T, T, T))))
    case _ => {
      System.err.println("* Warning: " + node.getNodeName + " cannot be an instance of CharacterData.")
      List()
    }
  }

  def getInsList2(): List[(String, AbsProperty)] =
      DOMNode.getInsList2() ++ List(
      // DOM Level 1
      ("data", AbsConstValue(PropValue(ObjectValue(Value(StrTop), T, T, T)))),
      ("length", AbsConstValue(PropValue(ObjectValue(Value(UInt), T, T, T))))
    )

  def getInsList(data: PropValue): List[(String, PropValue)] = List(("data", data))
}
