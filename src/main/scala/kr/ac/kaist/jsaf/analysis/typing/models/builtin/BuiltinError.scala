/*******************************************************************************
    Copyright (c) 2013-2014, S-Core.
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

object BuiltinError extends ModelData {
  /* error object(constructor) */
  val ErrConstLoc: Loc       = newSystemLoc("ErrConst", Recent)
  val EvalErrConstLoc: Loc   = newSystemLoc("EvalErrConst", Recent)
  val RangeErrConstLoc: Loc  = newSystemLoc("RangeErrConst", Recent)
  val RefErrConstLoc: Loc    = newSystemLoc("RefErrConst", Recent)
  val SyntaxErrConstLoc: Loc = newSystemLoc("SyntaxErrConst", Recent)
  val TypeErrConstLoc: Loc   = newSystemLoc("TypeErrConst", Recent)
  val URIErrConstLoc: Loc    = newSystemLoc("URIErrConst", Recent)

  /* error prototype object */
  val ErrProtoLoc:Loc       = newSystemLoc("ErrProto", Recent)
  val EvalErrProtoLoc:Loc   = newSystemLoc("EvalErrProto", Recent)
  val RangeErrProtoLoc: Loc = newSystemLoc("RangeErrProto", Recent)
  val RefErrProtoLoc: Loc   = newSystemLoc("RefErrProto", Recent)
  val SyntaxErrProtoLoc: Loc= newSystemLoc("SyntaxErrProto", Recent)
  val TypeErrProtoLoc: Loc  = newSystemLoc("TypeErrProto", Recent)
  val URIErrProtoLoc: Loc   = newSystemLoc("URIErrProto", Recent)

  /* error instance */
  val ErrLoc: Loc           = newSystemLoc("Err", Old)
  val EvalErrLoc: Loc       = newSystemLoc("EvalErr", Old)
  val RangeErrLoc: Loc      = newSystemLoc("RangeErr", Old)
  val RefErrLoc: Loc        = newSystemLoc("RefErr", Old)
  val SyntaxErrLoc: Loc     = newSystemLoc("SyntaxErr", Old)
  val TypeErrLoc: Loc       = newSystemLoc("TypeErr", Old)
  val URIErrLoc: Loc        = newSystemLoc("URIErr", Old)

  private val prop_err_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Error.constructor")),
    ("@construct",               AbsInternalFunc("Error.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_err_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(ErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("Error"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T)))),
    ("toString",             AbsBuiltinFunc("Error.prototype.toString", 0))
  )
  private val prop_err_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_eval_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("EvalError.constructor")),
    ("@construct",               AbsInternalFunc("EvalError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(EvalErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_eval_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(EvalErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("EvalError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_eval_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(EvalErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_range_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("RangeError.constructor")),
    ("@construct",               AbsInternalFunc("RangeError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(RangeErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_range_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(RangeErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("RangeError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_range_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(RangeErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_ref_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("ReferenceError.constructor")),
    ("@construct",               AbsInternalFunc("ReferenceError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(RefErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_ref_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(RefErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("ReferenceError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_ref_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(RefErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_sytax_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("SyntaxError.constructor")),
    ("@construct",               AbsInternalFunc("SyntaxError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(SyntaxErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_sytax_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(SyntaxErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("SyntaxError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_sytax_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(SyntaxErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_type_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("TypeError.constructor")),
    ("@construct",               AbsInternalFunc("TypeError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(TypeErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_type_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(TypeErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("TypeError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_typ_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(TypeErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  private val prop_uri_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("URIError.constructor")),
    ("@construct",               AbsInternalFunc("URIError.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(URIErrProtoLoc, F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )
  private val prop_uri_proro: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(URIErrConstLoc, F, F, F)))),
    ("name",                 AbsConstValue(PropValue(ObjectValue(AbsString.alpha("URIError"), T, F, T)))),
    ("message",              AbsConstValue(PropValue(ObjectValue(AbsString.alpha(""), T, F, T))))
  )
  private val prop_uri_ins: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Error")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(URIErrProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(T)))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ErrConstLoc, prop_err_const),        (ErrProtoLoc, prop_err_proto),        (ErrLoc, prop_err_ins),
    (EvalErrConstLoc, prop_eval_const),   (EvalErrProtoLoc, prop_eval_proto),   (EvalErrLoc, prop_eval_ins),
    (RangeErrConstLoc, prop_range_const), (RangeErrProtoLoc, prop_range_proto), (RangeErrLoc, prop_range_ins),
    (RefErrConstLoc, prop_ref_const),     (RefErrProtoLoc, prop_ref_proto),     (RefErrLoc, prop_ref_ins),
    (SyntaxErrConstLoc, prop_sytax_const),(SyntaxErrProtoLoc, prop_sytax_proto),(SyntaxErrLoc, prop_sytax_ins),
    (TypeErrConstLoc, prop_type_const),   (TypeErrProtoLoc, prop_type_proto),   (TypeErrLoc, prop_typ_ins),
    (URIErrConstLoc, prop_uri_const),     (URIErrProtoLoc, prop_uri_proro),     (URIErrLoc, prop_uri_ins)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("Error"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = ErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("Error.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs

          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              lset_this.foldLeft(h)((_h, l) => _h.update(l, h(l).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue)))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(lset_this)), ctx), (he, ctxe))
        })),
      ("Error.prototype.toString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val s_empty = AbsString.alpha("")
          val v_name = lset_this.foldLeft(ValueBot)((v, l) => v + Helper.Proto(h, l, AbsString.alpha("name")))
          val v_msg = lset_this.foldLeft(ValueBot)((v, l) => v + Helper.Proto(h, l, AbsString.alpha("message")))
          val s_1 =
            if (v_name.pv._1 </ UndefBot)
              AbsString.alpha("Error")
            else
              StrBot
          val s_2 = Helper.toString(PValue(UndefBot, v_name.pv._2, v_name.pv._3, v_name.pv._4, v_name.pv._5))
          val s_name = s_1 + s_2
          val s_3 =
            if (v_msg.pv._1 </ UndefBot)
              s_empty
            else
              StrBot
          val s_4 = Helper.toString(PValue(UndefBot, v_msg.pv._2, v_msg.pv._3, v_msg.pv._4, v_msg.pv._5))
          val s_msg = s_3 + s_4
          val s_5 =
            if (s_empty <= s_name)
              s_msg
            else
              StrBot
          val s_6 =
            if (s_empty <= s_msg)
              s_name
            else
              StrBot
          val s_7 = Operator.bopPlus(Operator.bopPlus(Value(s_name), Value(AbsString.alpha(": "))), Value(s_msg)).pv._5
          val s_ret = s_5 + s_6 + s_7

          if (s_ret </ StrBot)
            ((Helper.ReturnStore(h, Value(s_ret)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("EvalError"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = EvalErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("EvalError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = EvalErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),

      ("RangeError" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = RangeErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("RangeError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = RangeErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("ReferenceError" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = RefErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("ReferenceError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = RefErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),

      ("SyntaxError" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = SyntaxErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("SyntaxError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = SyntaxErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),

      ("TypeError" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = TypeErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("TypeError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = TypeErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),

      ("URIError" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = URIErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        })),
      ("URIError.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_arg = getArgValue(h, ctx, args, "0")
          val l_e = URIErrLoc
          val h_1 =
            if (Value(PValue(UndefBot, v_arg.pv._2, v_arg.pv._3, v_arg.pv._4, v_arg.pv._5), v_arg.locs) </ ValueBot) {
              val s = Helper.toString(Helper.toPrimitive_better(h, v_arg))
              h.update(l_e, h(l_e).update("message", PropValue(ObjectValue(s,BoolTrue,BoolFalse,BoolTrue))))
            }
            else
              HeapBot
          val h_2 =
            if (v_arg.pv._1 </ UndefBot)
              h
            else
              HeapBot
          ((Helper.ReturnStore(h_1 + h_2, Value(l_e)), ctx), (he, ctxe))
        }))
    )
  }

}
