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

object BuiltinNumber extends ModelData {

  val ConstLoc = newSystemLoc("NumberConst", Recent)
  val ProtoLoc = newSystemLoc("NumberProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",            AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",            AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",       AbsConstValue(PropValue(T))),
    ("@scope",            AbsConstValue(PropValueNullTop)),
    ("@function",         AbsInternalFunc("Number")),
    ("@construct",        AbsInternalFunc("Number.constructor")),
    ("@hasinstance",      AbsConstValue(PropValueNullTop)),
    ("prototype",         AbsConstValue(PropValue(ObjectValue(Value(ProtoLoc), F, F, F)))),
    ("length",            AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F)))),
    ("MAX_VALUE",         AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(Double.MaxValue), F, F, F)))),
    ("MIN_VALUE",         AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(java.lang.Double.MIN_VALUE), F, F, F)))),
    ("NaN",               AbsConstValue(PropValue(ObjectValue(NaN, F, F, F)))),
    ("NEGATIVE_INFINITY", AbsConstValue(PropValue(ObjectValue(NegInf, F, F, F)))),
    ("POSITIVE_INFINITY", AbsConstValue(PropValue(ObjectValue(PosInf, F, F, F))))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",         AbsConstValue(PropValue(AbsString.alpha("Number")))),
    ("@proto",         AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",    AbsConstValue(PropValue(BoolTrue))),
    ("@primitive",     AbsConstValue(PropValue(AbsNumber.alpha(+0)))),
    ("constructor",    AbsConstValue(PropValue(ObjectValue(ConstLoc, F, F, F)))),
    ("toString",       AbsBuiltinFunc("Number.prototype.toString", 0)),
    ("toLocaleString", AbsBuiltinFunc("Number.prototype.toLocaleString", 0)),
    ("valueOf",        AbsBuiltinFunc("Number.prototype.valueOf", 0)),
    ("toFixed",        AbsBuiltinFunc("Number.prototype.toFixed", 1)),
    ("toExponential",  AbsBuiltinFunc("Number.prototype.toExponential", 1)),
    ("toPrecision",    AbsBuiltinFunc("Number.prototype.toPrecision", 1))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ProtoLoc, prop_proto)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      "Number" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 15.7.1.1 Number( [value] )
          val v_1 = getArgValue(h, ctx, args, "0")
          val arg_length = getArgValue(h, ctx, args, "length").pv._4

          // If value is not supplied, +0 is returned.
          val value_1 =
            if (AbsNumber.alpha(0) <= arg_length) Value(AbsNumber.alpha(0))
            else ValueBot
          // Returns a Number value computed by ToNumber(value).
          val value_2 =
            if (AbsNumber.alpha(0) != arg_length && !(arg_length <= NumBot)) Value(Helper.toNumber(Helper.toPrimitive_better(h, v_1)))
            else ValueBot
          val value = value_1 + value_2

          ((Helper.ReturnStore(h, value), ctx), (he, ctxe))
        }),
      "Number.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          // 15.7.2.1 new Number( [value] )
          val v_1 = getArgValue(h, ctx, args, "0")
          val arg_length = getArgValue(h, ctx, args, "length").pv._4

          // [[PrimitiveValue]]
          val value_1 =
            if (AbsNumber.alpha(0) <= arg_length) AbsNumber.alpha(0)
            else NumBot
          val value_2 =
            if (AbsNumber.alpha(0) != arg_length && !(arg_length <= NumBot)) Helper.toNumber(Helper.toPrimitive_better(h, v_1))
            else NumBot
          val primitive_value = value_1 + value_2

          val h_1 = lset_this.foldLeft(h)((_h, l) => _h.update(l, Helper.NewNumber(primitive_value)))

          if (primitive_value </ NumBot)
            ((Helper.ReturnStore(h_1, Value(lset_this)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "Number.prototype.toString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val lset_num = lset_this.filter((l) => AbsString.alpha("Number") <= h(l)("@class")._2.pv._5)

          val es = notGenericMethod(h, lset_this, "Number")
          val (v, es2) =
            n_arglen.getAbsCase match {
              case AbsBot => (ValueBot, ExceptionBot)
              case _ => AbsNumber.getUIntSingle(n_arglen) match {
                case Some(n_arglen) if n_arglen == 0 =>
                  (Value(Helper.defaultToString(h, lset_num)), ExceptionBot)
                case Some(n_arglen) if n_arglen > 0 => {
                  val es =
                    if (BoolTrue <= Operator.bopGreater(getArgValue(h, ctx, args, "0"), Value(AbsNumber.alpha(36))).pv._3)
                      Set[Exception](RangeError)
                    else if (BoolTrue <= Operator.bopLess(getArgValue(h, ctx, args, "0"), Value(AbsNumber.alpha(2))).pv._3)
                      Set[Exception](RangeError)
                    else
                      ExceptionBot
                  (Value(StrTop), es)
                }
                case _ => {
                  (Value(StrTop), Set[Exception](RangeError))
                }
              }
            }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es ++ es2)
          if (v </ ValueBot)
            ((Helper.ReturnStore(h, v), ctx), (he + h_e, ctxe + ctx_e))
          else
            ((HeapBot, ContextBot), (he + h_e, ctxe + ctx_e))
        }),
      "Number.prototype.toLocaleString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val v_prim = lset_this.foldLeft(ValueBot)((_v, _l) => _v + h(_l)("@primitive")._2)
          val v = Value(Helper.toString(v_prim.pv))
          if (v </ ValueBot)
            ((Helper.ReturnStore(h, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "Number.prototype.valueOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val es = notGenericMethod(h, lset_this, "Number")
          val lset_num = lset_this.filter((l) => AbsString.alpha("Number") <= h(l)("@class")._2.pv._5)
          val n = lset_num.foldLeft[AbsNumber](NumBot)((_b, l) => _b + h(l)("@primitive")._2.pv._4)
          val (h_1, c_1) =
            if (n == NumBot)
              (HeapBot, ContextBot)
            else
              (Helper.ReturnStore(h, Value(n)), ctx)
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((h_1, c_1), (he + h_e, ctxe + ctx_e))
        }),
      "Number.prototype.toFixed" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val v_2 =
            if (UndefTop <= v_1.pv._1)
              Value(PValue(UndefBot, v_1.pv._2, v_1.pv._3, AbsNumber.alpha(0) + v_1.pv._4, v_1.pv._5), v_1.locs)
            else
              v_1
          val es =
            if (BoolTrue <= Operator.bopGreater(v_2, Value(AbsNumber.alpha(20))).pv._3)
              Set[Exception](RangeError)
            else if (BoolTrue <= Operator.bopLess(v_2, Value(AbsNumber.alpha(0))).pv._3)
              Set[Exception](RangeError)
            else
              ExceptionBot
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he + h_e, ctxe + ctx_e))
        }),
      "Number.prototype.toExponential" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val v_2 =
            if (UndefTop <= v_1.pv._1)
              Value(PValue(UndefBot, v_1.pv._2, v_1.pv._3, v_1.pv._4, v_1.pv._5), v_1.locs)
            else
              v_1
          val es =
            if (BoolTrue <= Operator.bopGreater(v_2, Value(AbsNumber.alpha(20))).pv._3)
              Set[Exception](RangeError)
            else if (BoolTrue <= Operator.bopLess(v_2, Value(AbsNumber.alpha(0))).pv._3)
              Set[Exception](RangeError)
            else
              ExceptionBot
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he + h_e, ctxe + ctx_e))
        }),
      "Number.prototype.toPrecision" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val v_2 =
            if (UndefTop <= v_1.pv._1)
              Value(PValue(UndefBot, v_1.pv._2, v_1.pv._3, v_1.pv._4, v_1.pv._5), v_1.locs)
            else
              v_1
          val es =
            if (BoolTrue <= Operator.bopGreater(v_2, Value(AbsNumber.alpha(21))).pv._3)
              Set[Exception](RangeError)
            else if (BoolTrue <= Operator.bopLess(v_2, Value(AbsNumber.alpha(1))).pv._3)
              Set[Exception](RangeError)
            else
              ExceptionBot
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he + h_e, ctxe + ctx_e))
        })
    )
  }

}
