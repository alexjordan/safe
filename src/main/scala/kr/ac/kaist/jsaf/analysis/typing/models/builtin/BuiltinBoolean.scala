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

package kr.ac.kaist.jsaf.analysis.typing.models.builtin

import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object BuiltinBoolean extends ModelData {

  val ConstLoc = newSystemLoc("BooleanConst", Recent)
  val ProtoLoc = newSystemLoc("BooleanProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Boolean")),
    ("@construct",               AbsInternalFunc("Boolean.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(ProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",         AbsConstValue(PropValue(AbsString.alpha("Boolean")))),
    ("@proto",         AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",    AbsConstValue(PropValue(BoolTrue))),
    ("constructor",    AbsConstValue(PropValue(ObjectValue(ConstLoc, F, F, F)))),
    ("toString",       AbsBuiltinFunc("Boolean.prototype.toString", 0)),
    ("valueOf",        AbsBuiltinFunc("Boolean.prototype.valueOf", 0))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ProtoLoc, prop_proto)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      "Boolean" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 15.6.1.1 Boolean(value)
          val v_1 = getArgValue(h, ctx, args, "0")
          val arg_length = getArgValue(h, ctx, args, "length").pv._4

          // Returns a Boolean value computed by ToBoolean(value).
          val value =
            if (!(arg_length <= NumBot)) Value(Helper.toBoolean(v_1))
            else ValueBot
          ((Helper.ReturnStore(h, value), ctx), (he, ctxe))
        }),
      "Boolean.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 15.6.2.1 new Boolean(value)
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val v_1 = getArgValue(h, ctx, args, "0")
          val arg_length = getArgValue(h, ctx, args, "length").pv._4

          // [[PrimitiveValue]]
          val primitive_value =
            if (!(arg_length <= NumBot)) Helper.toBoolean(v_1)
            else BoolBot

          val h_1 = lset_this.foldLeft(h)((_h, l) => _h.update(l, Helper.NewBoolean(primitive_value)))

          if (primitive_value </ BoolBot) {
            ((Helper.ReturnStore(h_1, Value(lset_this)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "Boolean.prototype.toString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val es = notGenericMethod(h, lset_this, "Boolean")

          val lset_bool = lset_this.filter((l) => AbsString.alpha("Boolean") <= h(l)("@class")._2.pv._5)
          val s = Helper.defaultToString(h, lset_bool)
          val (h_1, c_1) =
            if (s == StrBot) {
              (HeapBot, ContextBot)
            }
            else {
              (Helper.ReturnStore(h, Value(s)), ctx)
            }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((h_1, c_1), (h_e, ctx_e))
        }),
      "Boolean.prototype.valueOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val es = notGenericMethod(h, lset_this, "Boolean")

          val lset_bool = lset_this.filter((l) => AbsString.alpha("Boolean") <= h(l)("@class")._2.pv._5)
          val b = lset_bool.foldLeft[AbsBool](BoolBot)((_b, l) => _b + h(l)("@primitive")._2.pv._3)
          val (h_1, c_1) =
            if (b == BoolBot) {
              (HeapBot, ContextBot)
            }
            else {
              (Helper.ReturnStore(h, Value(b)), ctx)
            }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((h_1, c_1), (h_e, ctx_e))
        })
    )
  }

}
