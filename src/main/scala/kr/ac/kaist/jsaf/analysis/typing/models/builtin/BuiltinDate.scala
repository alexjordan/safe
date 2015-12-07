/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing.models.builtin

import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.bug_detector.Range15_9_5_43
import scala.collection.immutable.HashSet
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object BuiltinDate extends ModelData {

  val ConstLoc = newSystemLoc("DateConst", Recent)
  val ProtoLoc = newSystemLoc("DateProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Date")),
    ("@construct",               AbsInternalFunc("Date.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(ProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F)))),
    ("parse", AbsBuiltinFunc("Date.parse", 1)),
    ("UTC",   AbsBuiltinFunc("Date.UTC", 7)),
    ("now",   AbsBuiltinFunc("Date.now", 0))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",      AbsConstValue(PropValue(AbsString.alpha("Date")))),
    ("@proto",      AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("constructor", AbsConstValue(PropValue(ObjectValue(ConstLoc, F, F, F)))),
    ("toString",           AbsBuiltinFunc("Date.prototype.toString", 0)),
    ("toDateString",       AbsBuiltinFunc("Date.prototype.toDateString", 0)),
    ("toTimeString",       AbsBuiltinFunc("Date.prototype.toTimeString", 0)),
    ("toLocaleString",     AbsBuiltinFunc("Date.prototype.toLocaleString", 0)),
    ("toLocaleDateString", AbsBuiltinFunc("Date.prototype.toLocaleDateString", 0)),
    ("toLocaleTimeString", AbsBuiltinFunc("Date.prototype.toLocaleTimeString", 0)),
    ("valueOf",            AbsBuiltinFunc("Date.prototype.valueOf", 0)),
    ("getTime",            AbsBuiltinFunc("Date.prototype.getTime", 0)),
    ("getFullYear",        AbsBuiltinFunc("Date.prototype.getFullYear", 0)),
    ("getUTCFullYear",     AbsBuiltinFunc("Date.prototype.getUTCFullYear", 0)),
    ("getMonth",           AbsBuiltinFunc("Date.prototype.getMonth", 0)),
    ("getUTCMonth",        AbsBuiltinFunc("Date.prototype.getUTCMonth", 0)),
    ("getDate",            AbsBuiltinFunc("Date.prototype.getDate", 0)),
    ("getUTCDate",         AbsBuiltinFunc("Date.prototype.getUTCDate", 0)),
    ("getDay",             AbsBuiltinFunc("Date.prototype.getDay", 0)),
    ("getUTCDay",          AbsBuiltinFunc("Date.prototype.getUTCDay", 0)),
    ("getHours",           AbsBuiltinFunc("Date.prototype.getHours", 0)),
    ("getUTCHours",        AbsBuiltinFunc("Date.prototype.getUTCHours", 0)),
    ("getMinutes",         AbsBuiltinFunc("Date.prototype.getMinutes", 0)),
    ("getUTCMinutes",      AbsBuiltinFunc("Date.prototype.getUTCMinutes", 0)),
    ("getSeconds",         AbsBuiltinFunc("Date.prototype.getSeconds", 0)),
    ("getUTCSeconds",      AbsBuiltinFunc("Date.prototype.getUTCSeconds", 0)),
    ("getMilliseconds",    AbsBuiltinFunc("Date.prototype.getMilliseconds", 0)),
    ("getUTCMilliseconds", AbsBuiltinFunc("Date.prototype.getUTCMilliseconds", 0)),
    ("getTimezoneOffset",  AbsBuiltinFunc("Date.prototype.getTimezoneOffset", 0)),
    ("setTime",            AbsBuiltinFunc("Date.prototype.setTime", 1)),
    ("setMilliseconds",    AbsBuiltinFunc("Date.prototype.setMilliseconds", 1)),
    ("setUTCMilliseconds", AbsBuiltinFunc("Date.prototype.setUTCMilliseconds", 1)),
    ("setSeconds",         AbsBuiltinFunc("Date.prototype.setSeconds", 2)),
    ("setUTCSeconds",      AbsBuiltinFunc("Date.prototype.setUTCSeconds", 2)),
    ("setMinutes",         AbsBuiltinFunc("Date.prototype.setMinutes", 3)),
    ("setUTCMinutes",      AbsBuiltinFunc("Date.prototype.setUTCMinutes", 3)),
    ("setHours",           AbsBuiltinFunc("Date.prototype.setHours", 4)),
    ("setUTCHours",        AbsBuiltinFunc("Date.prototype.setUTCHours", 4)),
    ("setDate",            AbsBuiltinFunc("Date.prototype.setDate", 1)),
    ("setUTCDate",         AbsBuiltinFunc("Date.prototype.setUTCDate", 1)),
    ("setMonth",           AbsBuiltinFunc("Date.prototype.setMonth", 2)),
    ("setUTCMonth",        AbsBuiltinFunc("Date.prototype.setUTCMonth", 2)),
    ("setFullYear",        AbsBuiltinFunc("Date.prototype.setFullYear", 3)),
    ("setUTCFullYear",     AbsBuiltinFunc("Date.prototype.setUTCFullYear", 3)),
    ("toUTCString",        AbsBuiltinFunc("Date.prototype.toUTCString", 0)),
    ("toISOString",        AbsBuiltinFunc("Date.prototype.toISOString", 0)),
    ("toJSON",             AbsBuiltinFunc("Date.prototype.toJSON", 1))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ProtoLoc, prop_proto)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("Date" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val pv_1 = Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0"))
          val n_prim_3 =
            if (pv_1._5 </ StrBot)
            //parse
              NumTop
            else
              NumBot
          val pv_nonstr = PValue(pv_1._1, pv_1._2, pv_1._3, pv_1._4, StrBot)
          val n_prim_4 =
            if (pv_nonstr </ PValueBot)
              Helper.toNumber(pv_nonstr)
            else
              NumBot
          val n_prim = n_arglen.getAbsCase match {
            case AbsBot => NumBot
            case _ => AbsNumber.getUIntSingle(n_arglen) match {
              // 15.9.3.2 new Date(value)
              case Some(n) if n == 1 => (n_prim_3 + n_prim_4)
              // 15.9.3.1 new Date( year, month [, date [, hours [, minutes [, seconds [, ms ]]]]] )
              // 15.9.3.3 new Date()
              case Some(n) if n != 1 => NumTop
              case _ => NumTop
            }
          }

          val h_1 = lset_this.foldLeft(h)((_h, l) => _h.update(l, Helper.NewDate(Value(n_prim))))
          if (n_prim </ NumBot)
            ((Helper.ReturnStore(h_1, Value(lset_this)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot),(he,ctxe))
        })),
      ("Date.now" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.parse" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toDateString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toTimeString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toLocaleString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toLocaleDateString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toLocaleTimeString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.toUTCString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
        })),
      "Date.prototype.toISOString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_callee = getArgValue(h, ctx, args, "callee")._2
          val abstraction = (lset_callee.size > 1)
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val v = lset_this.foldLeft(ValueBot)((_v, l) => _v + h(l)("@primitive")._2)
          val num_v = v._1._4

          val v_1 =
            if ((num_v <> UInt) </ NumBot || (num_v <> NUInt) </ NumBot) {
              Value(StrTop)
            } else {
              ValueBot
            }

          // If the time value of this object is not a finite Number a RangeError exception is thrown.
          val es =
            if (PosInf <= num_v || NegInf <= num_v || NaN <= num_v) {
              if (Config.typingInterface != null)
                if(Shell.params.opt_DeveloperMode || !abstraction)
                  Config.typingInterface.signal(Config.typingInterface.getSpan, Range15_9_5_43, num_v.toString, null)
              HashSet[Exception](RangeError)
            } else {
              ExceptionBot
            }

          val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)

          val (h_1, ctx_1) =
            if (v_1 </ ValueBot) {
              (Helper.ReturnStore(h, v_1), ctx)
            } else {
              (HeapBot, ContextBot)
            }

          ((h_1, ctx_1), (he + he_1, ctxe + ctxe_1))
        }),
      ("Date.prototype.valueOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val v = lset_this.foldLeft(ValueBot)((_v, l) => _v + h(l)("@primitive")._2)
          if (v </ ValueBot)
            ((Helper.ReturnStore(h, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.getTime" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val v = lset_this.foldLeft(ValueBot)((_v, l) => _v + h(l)("@primitive")._2)
          if (v </ ValueBot)
            ((Helper.ReturnStore(h, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.getFullYear" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getMonth"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getDate"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getDay"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getHours"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getMinutes"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getSeconds"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getMilliseconds"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getTimezoneOffset" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCFullYear" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCMonth" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCDate" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCDay" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCHours" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCMinutes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCSeconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.getUTCMilliseconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(NumTop)), ctx), (he, ctxe))
        })),
      ("Date.prototype.setTime" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val time = getArgValue(h, ctx, args, "0")
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValue(time))))
          ((Helper.ReturnStore(h_1, time), ctx), (he, ctxe))
        })),
      ("Date.prototype.setMilliseconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setSeconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setMinutes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setMinutes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setHours" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setDate" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setMonth" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setFullYear" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCMilliseconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCSeconds" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCMinutes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCHours" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCDate" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCMonth" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Date.prototype.setUTCFullYear" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val h_1 = lset_this.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@primitive", PropValueNumTop)))
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(NumTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }))
    )
  }

}
