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

import scala.math.{min,max,floor, abs}
import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import scala.Some
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object BuiltinArray extends ModelData {

  val ConstLoc = newSystemLoc("ArrayConst", Recent)
  val ProtoLoc = newSystemLoc("ArrayProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Array")),
    ("@construct",               AbsInternalFunc("Array.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(ProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F)))),
    ("isArray",                  AbsBuiltinFunc("Array.isArray", 1))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Array")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(BoolTrue))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(ConstLoc, F, F, F)))),
    ("length",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0), F, F, F)))),
    ("toString",             AbsBuiltinFunc("Array.prototype.toString", 0)),
    ("toLocaleString",       AbsBuiltinFunc("Array.prototype.toLocaleString", 0)),
    ("concat",               AbsBuiltinFunc("Array.prototype.concat", 1)),
    ("join",                 AbsBuiltinFunc("Array.prototype.join", 1)),
    ("pop",                  AbsBuiltinFunc("Array.prototype.pop", 0)),
    ("push",                 AbsBuiltinFunc("Array.prototype.push", 1)),
    ("reverse",              AbsBuiltinFunc("Array.prototype.reverse", 0)),
    ("shift",                AbsBuiltinFunc("Array.prototype.shift", 0)),
    ("slice",                AbsBuiltinFunc("Array.prototype.slice", 2)),
    ("sort",                 AbsBuiltinFunc("Array.prototype.sort", 1)),
    ("splice",               AbsBuiltinFunc("Array.prototype.splice", 2)),
    ("unshift",              AbsBuiltinFunc("Array.prototype.unshift", 1)),
    ("indexOf",              AbsBuiltinFunc("Array.prototype.indexOf", 1)),
    ("lastIndexOf",          AbsBuiltinFunc("Array.prototype.lastIndexOf", 1)),
    ("every",                AbsBuiltinFunc("Array.prototype.every", 1)),
    ("some",                 AbsBuiltinFunc("Array.prototype.some", 1)),
    ("forEach",              AbsBuiltinFuncCallback("Array.prototype.forEach", 1)),
    ("map",                  AbsBuiltinFunc("Array.prototype.map", 1)),
    ("filter",               AbsBuiltinFunc("Array.prototype.filter", 1)),
    ("reduce",               AbsBuiltinFuncCallback("Array.prototype.reduce", 1)),
    ("reduceRight",          AbsBuiltinFuncCallback("Array.prototype.reduce", 1))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ProtoLoc, prop_proto)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("Array" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1)  = Helper.Oldify(h, ctx, addr1)
          val v_1 = getArgValue(h_1, ctx_1, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h_1, ctx_1, args, "length"))

          // case for "new Array(n)"
          val (h_arg_1, es1) =
            if(v_1 </ ValueBot) {
              val es = if (AbsNumber.isUIntOrBot(v_1.pv._4)) ExceptionBot
                       else Set[Exception](RangeError)
              val v_notNum = Value(PValue(v_1.pv._1,v_1.pv._2,v_1.pv._3,NumBot,v_1.pv._5), v_1.locs)
              // case for new Array("value")
              val o_notNum =
                if (v_notNum </ ValueBot) {
                  Helper.NewArrayObject(AbsNumber.alpha(1)).
                    update("0", PropValue(ObjectValue(v_notNum, BoolTrue, BoolTrue, BoolTrue)))
                }
                else
                  Obj.bottom
              // case for new Array(len)
              val o_num =
                if (v_1.pv._4 </ NumBot) {
                  Helper.NewArrayObject(Operator.ToUInt32(Value(v_1.pv._4)))
                }
                else
                  Obj.bottom

              (h_1.update(l_r, o_notNum + o_num), es)
            }
            else {
              (HeapBot, ExceptionBot)
            }

          val (h_2, es2) = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n) if n == 1 =>
              (h_arg_1, es1)
            case Some(n) if n != 1 => {
              // case for "new Array([v_1[, v_2[, ...])
              val o = (0 until n.toInt).foldLeft(Helper.NewArrayObject(n_arglen))((_o, i) =>
                _o.update(i.toString, PropValue(ObjectValue(getArgValue(h_1, ctx_1, args, i.toString), BoolTrue, BoolTrue, BoolTrue))))

              (h_1.update(l_r, o), ExceptionBot)

            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => (HeapBot, ExceptionBot)
              case _ =>
                val o = Helper.NewArrayObject(UInt).
                  update(NumStr, PropValue(ObjectValue(getArgValueAbs(h_1, ctx_1, args, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                val h_uint = h_1.update(l_r, o)
                (h_arg_1 + h_uint, es1)
            }
          }

          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es2)

          if (!(h_2 <= HeapBot))
            ((Helper.ReturnStore(h_2, Value(l_r)), ctx), (he + h_e, ctxe + ctx_e))
          else
            ((HeapBot, ContextBot), (he + h_e, ctxe + ctx_e))
        })),
      ("Array.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val v_1 = getArgValue(h, ctx, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))

          // case for "new Array(n)"
          val (h_arg_1, es1) =
            if(v_1 </ ValueBot) {
              val es = if (AbsNumber.isUIntOrBot(v_1.pv._4)) ExceptionBot
                       else Set[Exception](RangeError)
              val v_notNum = Value(PValue(v_1.pv._1,v_1.pv._2,v_1.pv._3,NumBot,v_1.pv._5), v_1.locs)
              // case for new Array("value")
              val o_notNum =
                if (v_notNum </ ValueBot) {
                  Helper.NewArrayObject(AbsNumber.alpha(1)).
                    update("0", PropValue(ObjectValue(v_notNum, BoolTrue, BoolTrue, BoolTrue)))
                }
                else
                  Obj.bottom
              // case for new Array(len)
              val o_num =
                if (v_1.pv._4 </ NumBot) {
                  Helper.NewArrayObject(Operator.ToUInt32(Value(v_1.pv._4)))
                }
                else
                  Obj.bottom

              (lset_this.foldLeft(h)((_h,l) => _h.update(l, o_notNum + o_num)), es)
            }
            else {
              (HeapBot, ExceptionBot)
            }

          val (h_2, es2) = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n) if n == 1 =>
              (h_arg_1, es1)
            case Some(n) if n != 1 => {
              // case for "new Array([v_1[, v_2[, ...])
              val o = (0 until n.toInt).foldLeft(Helper.NewArrayObject(n_arglen))((_o, i) =>
                _o.update(i.toString, PropValue(ObjectValue(getArgValue(h, ctx, args, i.toString), BoolTrue, BoolTrue, BoolTrue))))
              (lset_this.foldLeft(h)((_h,l) => _h.update(l, o)), ExceptionBot)
            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => (HeapBot, ExceptionBot)
              case _ =>
                val o = Helper.NewArrayObject(UInt).
                  update(NumStr, PropValue(ObjectValue(getArgValueAbs(h, ctx, args, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                val h_uint = lset_this.foldLeft(h)((_h,l) => _h.update(l, o))
                (h_arg_1 + h_uint, es1)
            }
          }

          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es2)

          if (!(h_2 <= HeapBot))
            ((Helper.ReturnStore(h_2, Value(lset_this)), ctx), (he + h_e, ctxe + ctx_e))
          else
            ((HeapBot, ContextBot), (he + h_e, ctxe + ctx_e))
        })),
      ("Array.isArray" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val b_1 =
            if (v.pv </ PValueBot) BoolFalse
            else BoolBot
          val b_2 = v.locs.foldLeft[AbsBool](BoolBot)((_b, l) => {
            // XXX : Check whether it is correct or not
            if (!h.domIn(l)) BoolBot
            else  {
              val _b1 =
                if (AbsString.alpha("Array") <= h(l)("@class")._2.pv._5) BoolTrue
                else BoolBot
              val _b2 =
                if (AbsString.alpha("Array") </ h(l)("@class")._2.pv._5) BoolFalse
                else BoolBot
              _b + _b1 + _b2}})
          val b = b_1 + b_2
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.toString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val s_sep = AbsString.alpha(",")
          val n_len = Operator.ToUInt32(lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h, l, AbsString.alpha("length"))))
          val s = AbsNumber.getUIntSingle(n_len) match {
            case Some(n) if n == 0 => AbsString.alpha("")
            case Some(n) if n > 0 => {
              val v_f = lset_this.foldLeft(ValueBot)((_v, l) =>_v + Helper.Proto(h, l, AbsString.alpha("0")))
              val v_f2 = Value(PValue(UndefBot,NullBot,v_f.pv._3,v_f.pv._4,v_f.pv._5), v_f.locs)
              val s_first =
                if (v_f.pv._1 </ UndefBot || v_f.pv._2 </ NullBot)
                  AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h, v_f2))
                else
                  Helper.toString(Helper.toPrimitive_better(h, v_f))
              (1 until n.toInt).foldLeft(s_first)((_s, i) =>{
                val v_i = lset_this.foldLeft(ValueBot)((_v, l) =>_v + Helper.Proto(h, l, AbsString.alpha(i.toString)))
                val v_i2 = Value(PValue(UndefBot,NullBot,v_i.pv._3,v_i.pv._4,v_i.pv._5), v_i.locs)
                val s_i =
                  if (v_i.pv._1 </ UndefBot || v_i.pv._2 </ NullBot)
                    AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h, v_i2))
                  else
                    Helper.toString(Helper.toPrimitive_better(h, v_i))
                _s.concat(s_sep).concat(s_i)
              })
            }
            case _ => n_len.getAbsCase match {
              case AbsBot => StrBot
              case _ => StrTop
            }
          }
          if (s </ StrBot)
            ((Helper.ReturnStore(h, Value(s)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
        "Array.prototype.toLocaleString" -> (
          (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
            val v_this = h(SinglePureLocalLoc)("@this")._2

            // Get a new address
            val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
            val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
            if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
            val addr_env = (cp._1._1, set_addr.head)
            val addr1 = cfg.getAPIAddress(addr_env, 0)
            val addr2 = cfg.getAPIAddress(addr_env, 1)

            // 1. Let array be the result of calling ToObject passing the this value as the argument.
            val (v_this2, h_1, ctx_1, es_1) = Helper.toObject(h, ctx, v_this, addr1)
            val lset_this = v_this2.locs

            // 2. Let arrayLen be the result of calling the [[Get]] internal method of array with argument "length".
            val v_len = lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, AbsString.alpha("length")))
            // 3. Let len be ToUint32(arrayLen).
            val n_len = Operator.ToUInt32(v_len)
            val s_sep = AbsString.alpha(",")

            val (h_2, ctx_2, es_2, s) = n_len.getSingle match {
              case Some(n) if n == 0 => (h_1, ctx_1, ExceptionBot, AbsString.alpha(""))
              case Some(n) if n > 0 => {
                val v_f = lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, AbsString.alpha("0")))
                val v_f2 = Value(PValue(UndefBot, NullBot, v_f.pv._3, v_f.pv._4, v_f.pv._5), v_f.locs)
                val s_first =
                  if (v_f.pv._1 </ UndefBot || v_f.pv._2 </ NullBot)
                    AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h_1, v_f2))
                  else
                    Helper.toString(Helper.toPrimitive_better(h_1, v_f))

                // b. Let func be the result of calling the [[Get]] internal method of elementObj with argument "toLocaleString".
                val func = v_f.locs.foldLeft(ValueBot)((S, l) => S + Helper.Proto(h_1, l, AbsString.alpha("toLocaleString")))
                val notfn = func.locs.filter(l => BoolFalse <= Helper.IsCallable(h_1, l))
                // c. If IsCallable(func) is false, throw a TypeError exception.
                val es_1 =
                  if (!notfn.isEmpty || func.pv </ PValueBot) {
                    Set[Exception](TypeError)
                  } else {
                    ExceptionBot
                  }

                val (s, es_2) = (1 until n.toInt).foldLeft((s_first, es_1))((_s, i) => {
                  val v_i = lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h, l, AbsString.alpha(i.toString)))
                  val v_i2 = Value(PValue(UndefBot, NullBot, v_i.pv._3, v_i.pv._4, v_i.pv._5), v_i.locs)
                  val s_i =
                    if (v_i.pv._1 </ UndefBot || v_i.pv._2 </ NullBot)
                      AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h, v_i2))
                    else
                      Helper.toString(Helper.toPrimitive_better(h, v_i))
                  // ii. Let func be the result of calling the [[Get]] internal method of elementObj with argument "toLocaleString".
                  val func = v_i.locs.foldLeft(ValueBot)((S, l) => S + Helper.Proto(h_1, l, AbsString.alpha("toLocaleString")))
                  val notfn = func.locs.filter(l => BoolFalse <= Helper.IsCallable(h_1, l))
                  // iii. If IsCallable(func) is false, throw a TypeError exception.
                  val es_i =
                    if (!notfn.isEmpty || func.pv </ PValueBot) {
                      Set[Exception](TypeError)
                    } else {
                      ExceptionBot
                    }

                  (_s._1.concat(s_sep).concat(s_i), _s._2 ++ es_i)
                })

                (h_1, ctx_1, es_2, s)
              }
              case None if n_len <= NumBot => (HeapBot, ContextBot, ExceptionBot, StrBot)
              case _ => {
                // 5. If len is zero, return the empty String.
                // 6. Let firstElement be the result of calling the [[Get]] internal method of array with argument "0".
                val elements_1 =
                  if (BoolTrue <= Operator.bopLessEq(Value(AbsNumber.alpha(1)), Value(n_len)).pv._3) {
                    lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, AbsString.NumTop))
                  } else {
                    ValueBot
                  }
                // a. Let elementObj be ToObject(firstElement).
                val (elements_2, h_2, ctx_2, es_2) =
                  if (elements_1 </ ValueBot) {
                    Helper.toObject(h_1, ctx_1, elements_1, addr2)
                  } else {
                    (ValueBot, h_1, ctx_1, ExceptionBot)
                  }

                // ii. Let func be the result of calling the [[Get]] internal method of elementObj with argument "toLocaleString".
                val func = elements_2.locs.foldLeft(ValueBot)((S, l) => S + Helper.Proto(h_2, l, AbsString.alpha("toLocaleString")))

                // iii. If IsCallable(func) is false, throw a TypeError exception.
                val notfn = func.locs.filter(l => BoolFalse <= Helper.IsCallable(h_2, l))
                val es_3 =
                  if (!notfn.isEmpty) {
                    Set[Exception](TypeError)
                  } else {
                    ExceptionBot
                  }

                val es = es_2 ++ es_3

                (h_2, ctx_2, es, StrTop)
              }
            }

            val (h_3, ctx_3) =
              if (s </ StrBot) {
                (Helper.ReturnStore(h_2, Value(s)), ctx_2)
              } else {
                (HeapBot, ContextBot)
              }
            val es = es_1 ++ es_2
            val (h_e, ctx_e) = Helper.RaiseException(h_3, ctx_3, es)

            ((h_3, ctx_3), (he + h_e, ctxe + ctx_e))
          }),
      ("Array.prototype.concat" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1)  = Helper.Oldify(h, ctx, addr1)
          val lset_this = h_1(SinglePureLocalLoc)("@this")._2.locs

          val n_arglen = Operator.ToUInt32(getArgValue(h_1, ctx_1, args, "length"))

          val o = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n_arg) => {
              val elem_list = (0 until n_arg.toInt).foldLeft[List[Value]](List(Value(lset_this)))((list, i) =>
                list :+ getArgValue(h_1, ctx_1, args, i.toString))
              val obj = Helper.NewArrayObject(AbsNumber.alpha(0))
              val index = AbsNumber.alpha(0)
              val (obj_1, len) = elem_list.foldLeft((obj, index))((oi, elem) => {
                val lset_array = elem.locs.filter((l) => AbsString.alpha("Array") <= h(l)("@class")._2.pv._5)
                val lset_narray = elem.locs.filter((l) => AbsString.alpha("Array") != h(l)("@class")._2.pv._5)
                val v_narray = Value(elem.pv, lset_narray)
                val o = oi._1
                val index = oi._2
                val (o_1, n_index_1) =
                  if (!lset_array.isEmpty) {
                    lset_array.foldLeft[(Obj,AbsNumber)]((Obj.bottom, NumBot))((_oi, l) => {
                      val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                      val __o = AbsNumber.getUIntSingle(n_len) match {
                        case Some(n) => {
                          (0 until n.toInt).foldLeft(o)((o_new, i)=>
                            o_new.update(Helper.toString(Operator.bopPlus(Value(index), Value(AbsNumber.alpha(i))).pv),
                              PropValue(ObjectValue(Helper.Proto(h_1, l, AbsString.alpha(i.toString)),BoolTrue,BoolTrue,BoolTrue))))
                        }
                        case _ => n_len.getAbsCase match {
                          case AbsBot => Obj.bottom
                          case _ =>
                            val v_all = Helper.Proto(h_1, l, NumStr)
                            o.update(NumStr, PropValue(ObjectValue(v_all,BoolTrue,BoolTrue,BoolTrue)))
                        }
                      }
                      val __i = Operator.bopPlus(Value(index), Value(n_len)).pv._4
                      (_oi._1 + __o , _oi._2 + __i)
                    })
                  }
                  else
                    (Obj.bottom, NumBot)
                val (o_2, n_index_2) =
                  if (v_narray </ ValueBot) {
                    val _o = o.update(Helper.toString(PValue(index)), PropValue(ObjectValue(elem, BoolTrue, BoolTrue, BoolTrue)))
                    val _i = Operator.bopPlus(Value(index), Value(AbsNumber.alpha(1))).pv._4
                    (_o, _i)
                  }
                  else
                    (Obj.bottom, NumBot)
                (o_1 + o_2, n_index_1 + n_index_2)})
              obj_1.update("length", PropValue(ObjectValue(Value(len), BoolTrue, BoolFalse, BoolFalse)))
            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => Obj.bottom
              case _ =>
                val v_all = Value(lset_this) + getArgValueAbs(h_1, ctx_1, args, NumStr)
                val lset_array = v_all.locs.filter((l) => AbsString.alpha("Array") <= h(l)("@class")._2.pv._5)
                val v_array = lset_array.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, NumStr))
                Helper.NewArrayObject(UInt).update(NumStr, PropValue(ObjectValue(v_all + v_array, BoolTrue,BoolTrue,BoolTrue)))
            }
          }
          if (o </ Obj.bottom){
            val h_2 = h_1.update(l_r, o)
            ((Helper.ReturnStore(h_2, Value(l_r)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.join" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val v_sep = getArgValue(h, ctx, args, "0")
          val v_sep2 = Value(PValue(UndefBot,v_sep.pv._2,v_sep.pv._3,v_sep.pv._4,v_sep.pv._5), v_sep.locs)
          val s_sep =
            if (v_sep.pv._1 </ UndefBot)
              AbsString.alpha(",") + Helper.toString(Helper.toPrimitive_better(h, v_sep2))
            else
              Helper.toString(Helper.toPrimitive_better(h, v_sep))

          val n_len = Operator.ToUInt32(lset_this.foldLeft(ValueBot)((_v, l) =>
            _v + Helper.Proto(h, l, AbsString.alpha("length"))))

          val s = AbsNumber.getUIntSingle(n_len) match {
            case Some(n) if n == 0 => AbsString.alpha("")
            case Some(n) if n > 0 => {
              val v_f = lset_this.foldLeft(ValueBot)((_v, l) =>_v + Helper.Proto(h, l, AbsString.alpha("0")))
              val v_f2 = Value(PValue(UndefBot,NullBot,v_f.pv._3,v_f.pv._4,v_f.pv._5), v_f.locs)
              val s_first =
                if (v_f.pv._1 </ UndefBot || v_f.pv._2 </ NullBot)
                  AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h, v_f2))
                else
                  Helper.toString(Helper.toPrimitive_better(h, v_f))
              (1 until n.toInt).foldLeft(s_first)((_s, i) =>{
                val v_i = lset_this.foldLeft(ValueBot)((_v, l) =>_v + Helper.Proto(h, l, AbsString.alpha(i.toString)))
                val v_i2 = Value(PValue(UndefBot,NullBot,v_i.pv._3,v_i.pv._4,v_i.pv._5), v_i.locs)
                val s_i =
                  if (v_i.pv._1 </ UndefBot || v_i.pv._2 </ NullBot)
                    AbsString.alpha("") + Helper.toString(Helper.toPrimitive_better(h, v_i2))
                  else
                    Helper.toString(Helper.toPrimitive_better(h, v_i))
                _s.concat(s_sep).concat(s_i)
              })
            }
            case _ => StrTop
          }

          if (s </ StrBot)
            ((Helper.ReturnStore(h, Value(s)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.pop" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_len = Operator.ToUInt32(lset_this.foldLeft(ValueBot)((_v, l) =>
            _v + Helper.Proto(h, l, AbsString.alpha("length"))))

          val (h_1, v) = lset_this.foldLeft((h, ValueBot))((hv, l) => {
            if (!(hv._1 <= HeapBot)) {
              val n_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
              val (_h, _v) = AbsNumber.getUIntSingle(n_len) match {
                case Some(n) if n == 0 => {
                  val __h = Helper.PropStore(hv._1, l, AbsString.alpha("length"), Value(AbsNumber.alpha(0)))
                  (__h, Value(UndefTop))
                }
                case Some(n) if n > 0 => {
                  val __v = Helper.Proto(hv._1, l,  AbsString.alpha((n-1).toInt.toString))
                  val __h = Helper.Delete(hv._1, l, AbsString.alpha((n-1).toInt.toString))._1
                  (Helper.PropStore(__h, l, AbsString.alpha("length"), Value(AbsNumber.alpha((n-1)))), __v)
                }
                case _ => n_len.getAbsCase match {
                  case AbsBot =>
                    (HeapBot, ValueBot)
                  case _ =>
                    val __v = Helper.Proto(hv._1, l, NumStr)
                    val __h = Helper.Delete(hv._1, l, NumStr)._1
                    (Helper.PropStore(__h, l, AbsString.alpha("length"), Value(UInt)), __v)
                }
              }
              (_h, hv._2 + _v)
            }
            else {
              (HeapBot, ValueBot)
            }
          })
          if (v </ ValueBot)
            ((Helper.ReturnStore(h_1, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.push" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val (h_1, v) = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n_arg) => {
              lset_this.foldLeft((h, ValueBot))((hv, l) => {
                if (!(hv._1 <= HeapBot)) {
                  val n_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
                  AbsNumber.getUIntSingle(n_len) match {
                    case Some(n) => {
                      val _h = (0 until n_arg.toInt).foldLeft(hv._1)((__h, i) =>
                        Helper.PropStore(__h, l, AbsString.alpha((i+n).toInt.toString), getArgValue(h, ctx, args, (i.toString))))
                      val _v = Value(AbsNumber.alpha(n_arg+n))
                      val _h1 = Helper.PropStore(_h, l, AbsString.alpha("length"), _v)
                      (_h1, hv._2 + _v)
                    }
                    case _ => n_len.getAbsCase match {
                      case AbsBot => (HeapBot, ValueBot)
                      case _ =>
                        val v_argall = getArgValueAbs(h, ctx, args, NumStr)
                        (Helper.PropStore(hv._1, l, NumStr, v_argall), Value(UInt))
                    }
                  }
                }
                else {
                  (HeapBot, ValueBot)
                }
              })
            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => (HeapBot, ValueBot)
              case _ =>
                val v_argall = getArgValueAbs(h, ctx, args, NumStr)
                (lset_this.foldLeft(h)((_h, l) => Helper.PropStore(_h, l, NumStr, v_argall)), Value(UInt))
            }
          }
          if (v </ ValueBot)
            ((Helper.ReturnStore(h_1, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.reverse" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val h_1 = lset_this.foldLeft(h)((_hh, l) => {
            val n_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
            AbsNumber.getUIntSingle(n_len) match {
              case Some(n) => {
                (0 until floor(n/2).toInt).foldLeft(_hh)((_h, i) =>{
                  val s_low = AbsString.alpha(i.toString)
                  val s_up = AbsString.alpha((n-i-1).toInt.toString)
                  val v_low = Helper.Proto(_h, l, s_low)
                  val v_up = Helper.Proto(_h, l, s_up)
                  val b_low = Helper.HasProperty(_h, l, s_low)
                  val b_up = Helper.HasProperty(_h, l, s_up)
                  val _h1 =
                    if (BoolTrue <= b_low && BoolTrue <= b_up) {
                      val __h1 = Helper.PropStore(_h, l, s_low, v_up)
                      Helper.PropStore(__h1, l, s_up, v_low)
                    }
                    else  {
                      HeapBot
                    }
                  val _h2 =
                    if (BoolFalse <= b_low && BoolTrue <= b_up) {
                      val __h1 = Helper.PropStore(_h, l, s_low, v_up)
                      Helper.Delete(__h1, l, s_up)._1
                    }
                    else  {
                      HeapBot
                    }
                  val _h3 =
                    if (BoolTrue <= b_low && BoolFalse <= b_up) {
                      val __h1 = Helper.PropStore(_h, l, s_up,  v_low)
                      Helper.Delete(__h1, l, s_low)._1
                    }
                    else  {
                      HeapBot
                    }
                  val _h4 =
                    if (BoolFalse <= b_low && BoolFalse <= b_up)  _h
                    else  {
                      HeapBot
                    }
                  _h1 + _h2 + _h3 + _h4
                })
              }
              case _ => n_len.getAbsCase match {
                case AbsBot => HeapBot
                case _ => 
                  val _h1 = Helper.PropStore(_hh, l, NumStr, Helper.Proto(_hh, l, NumStr))
                  val _h2 = Helper.Delete(_h1, l, NumStr)._1
                  (_h1 + _h2)
              }
            }
          })
          if (!(h_1 <= HeapBot))
            ((Helper.ReturnStore(h_1, Value(lset_this)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.shift" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val (h_1, v) = lset_this.foldLeft((h,ValueBot))((_hv, l) => {
            val n_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
            val _h = _hv._1
            val _v = _hv._2
            AbsNumber.getUIntSingle(n_len) match {
              case Some(n) => {
                if (n == 0) {
                  (Helper.PropStore(_h, l, AbsString.alpha("length"), Value(AbsNumber.alpha(0))), _v + Value(UndefTop))
                }
                else {
                  val v_first = Helper.Proto(_h, l, AbsString.alpha("0"))
                  val _h1 = (1 until n.toInt).foldLeft(_h)((__h, i) => {
                    val s_from = AbsString.alpha(i.toString)
                    val s_to = AbsString.alpha((i-1).toString)
                    val b = Helper.HasProperty(__h, l, s_from)
                    val __h1 =
                      if (BoolTrue <= b)  Helper.PropStore(__h, l, s_to, Helper.Proto(__h, l, s_from))
                      else HeapBot
                    val __h2 =
                      if (BoolFalse <= b)  Helper.Delete(__h, l, s_to)._1
                      else HeapBot
                    __h1 + __h2
                  })
                  val _h2 = Helper.Delete(_h1, l, AbsString.alpha((n-1).toInt.toString))._1
                  val _h3 = Helper.PropStore(_h2, l, AbsString.alpha("length"), Value(AbsNumber.alpha(n-1)))
                  (_h3, _v + v_first)
                }
              }
              case _ => n_len.getAbsCase match {
                case AbsBot => (HeapBot, ValueBot)
                case _ =>
                  val _v = Helper.Proto(_h, l, NumStr)
                  val _h1 = Helper.Delete(_h, l, NumStr)._1
                  val _h2 = Helper.PropStore(_h1, l, AbsString.alpha("length"), Value(UInt))
                  (_h2, _v)
              }
            }
          })
          if (v </ ValueBot)
            ((Helper.ReturnStore(h_1, v), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.slice" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val lset_this = h_1(SinglePureLocalLoc)("@this")._2.locs
          val n_start = Operator.ToInteger(getArgValue(h_1, ctx_1, args, "0"))
          val v_end = getArgValue(h, ctx, args, "1")
          //val n_end = Operator.ToInteger(getArgValue(h_1, ctx_1, "1"))

          val o = n_start.getSingle match {
            case (Some(start)) =>
              lset_this.foldLeft(Obj.bottom)((_o, l) => {
                val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                val n_end =
                  if (v_end.pv._1 </ UndefBot)  n_len + v_end.pv._4
                  else Operator.ToInteger(v_end)
                _o + (n_end.getSingle match {
                  case Some(end) =>
                    AbsNumber.getUIntSingle(n_len) match {
                      case Some(n) => {
                        val from =
                          if (start < 0) max(n + start, 0).toInt
                          else min(start, n).toInt
                        val to =
                          if (end < 0) max(n + end, 0).toInt
                          else min(end, n).toInt
                        val span = max(to-from, 0)
                        val o_new = Helper.NewArrayObject(AbsNumber.alpha(span))
                        (0 until span).foldLeft(o_new)((__o, i) => {
                          val b = Helper.HasProperty(h_1, l, AbsString.alpha(i.toString))
                          val _o1 =
                            if (BoolTrue <= b)
                              __o.update(AbsString.alpha(i.toString),
                                PropValue(ObjectValue(Helper.Proto(h_1, l, AbsString.alpha((from+i).toString)),BoolTrue,BoolTrue,BoolTrue)))
                            else Obj.bottom
                          val _o2 =
                            if (BoolFalse <= b) __o
                            else Obj.bottom
                          _o1 + _o2 })
                      }
                      case _ => n_len.getAbsCase match {
                        case AbsBot => Obj.bottom
                        case _ =>
                          val o_new = Helper.NewArrayObject(UInt)
                          o_new.update(NumStr, PropValue(ObjectValue(Helper.Proto(h_1, l, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                      }
                    }
                  case None =>
                    if (n_end <= NumBot)
                      Obj.bottom
                    else
                      lset_this.foldLeft(Obj.bottom)((_o, l) => {
                        val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                        n_len match {
                          case NumBot => Obj.bottom
                          case _ =>
                            val o_new = Helper.NewArrayObject(UInt)
                            o_new.update(NumStr, PropValue(ObjectValue(Helper.Proto(h_1, l, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                        } })
                })
              })
            case _ =>
              if (n_start <= NumBot)
                Obj.bottom
              else {
                lset_this.foldLeft(Obj.bottom)((_o, l) => {
                  val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                  n_len match {
                    case NumBot => Obj.bottom
                    case _ =>
                      val o_new = Helper.NewArrayObject(UInt)
                      o_new.update(NumStr, PropValue(ObjectValue(Helper.Proto(h_1, l, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                  }
                })
              }
          }
          if (o </ Obj.bottom) {
            val h_2 = h_1.update(l_r, o)
            ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Array.prototype.sort" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this: LocSet = h(SinglePureLocalLoc)("@this")._2.locs
          // top value
          val h_1 = lset_this.foldLeft(h)((_h, l) => {
            Helper.PropStoreWeak(_h, l, NumStr, Helper.Proto(h, l, NumStr))
          })
          ((Helper.ReturnStore(h_1, Value(lset_this)), ctx), (he, ctxe))
      })),
      "Array.prototype.splice" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val lset_this = h_1(SinglePureLocalLoc)("@this")._2.locs

          val n_arglen = Operator.ToUInt32(getArgValue(h_1, ctx_1, args, "length"))
          val n_start = Operator.ToInteger(getArgValue(h_1, ctx_1, args, "0"))
          val n_count = Operator.ToInteger(getArgValue(h_1, ctx_1, args, "1"))

          AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n) if n == 1 => System.err.println("WARNING: splice called with 1 arg, validate JS3/6 compatible")
            case _ => ()
          }

          val (h_2, o) = (n_start.getSingle, n_count.getSingle) match {
            case (Some(start), Some(count)) =>
              lset_this.foldLeft((HeapBot, Obj.bottom))((_ho, l) => {
                val _h = _ho._1
                val _o = _ho._2
                val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                AbsNumber.getUIntSingle(n_len) match {
                  case Some(n_len) => {
                    val actualStart =
                      if (start < 0) max(n_len + start, 0).toInt
                      else min(start, n_len).toInt
                    // for browser compatibility
                    val actualDeleteCount = AbsNumber.getUIntSingle(n_arglen) match {
                      case Some(n_arglen) if n_arglen==1 =>  n_len.toInt 
                      case _ => min(max(count, 0), n_len - actualStart).toInt
                    }
                    val o_new = Helper.NewArrayObject(AbsNumber.alpha(actualDeleteCount))
                    val o_1 = (0 until actualDeleteCount).foldLeft(o_new)((__o, i) => {
                      val b = Helper.HasProperty(h_1, l, AbsString.alpha(i.toString))
                      val _o1 =
                        if (BoolTrue <= b)
                          __o.update(AbsString.alpha(i.toString),
                            PropValue(ObjectValue(Helper.Proto(h_1, l, AbsString.alpha((actualStart+i).toString)),BoolTrue,BoolTrue,BoolTrue)))
                        else Obj.bottom
                      val _o2 =
                        if (BoolFalse <= b) __o
                        else Obj.bottom
                      _o1 + _o2
                    })
                    val _h1 = AbsNumber.getUIntSingle(n_arglen) match {
                      case Some(n_arglen) => {
                        val add_count = if(0 <= (n_arglen.toInt - 2)) n_arglen.toInt - 2 else 0
                        val move_start = actualStart + actualDeleteCount
                        if (add_count < actualDeleteCount) {
                          val __h1 = (move_start.toInt until n_len.toInt).foldLeft(h_1)((__h, i) => {
                            val s_from = AbsString.alpha(i.toString)
                            val s_to = AbsString.alpha((i - actualDeleteCount+add_count).toInt.toString)
                            val v = Helper.Proto(__h, l, s_from)
                            val b = Helper.HasProperty(__h, l, s_from)
                            val __h1 =
                              if (BoolTrue <= b) Helper.PropStore(__h, l, s_to, v)
                              else HeapBot
                            val __h2 =
                              if (BoolFalse <= b) Helper.Delete(__h, l, s_to)._1
                              else HeapBot
                            __h1 + __h2
                          })
                          val __h2 = (0 until add_count).foldLeft(__h1)((__h, i) =>
                            Helper.PropStore(__h, l, AbsString.alpha((actualStart + i).toInt.toString), getArgValue(__h, ctx_1, args, (i+2).toString)))
                          val new_length = n_len + add_count - actualDeleteCount
                          val k = n_len;
                          val ss = n_len - actualDeleteCount + add_count
                          val __h3 = (ss.toInt until k.toInt).foldLeft(__h2)((__h, i) =>
                            Helper.Delete(__h, l, AbsString.alpha(i.toString))._1)
                          Helper.PropStore(__h3, l,  AbsString.alpha("length"), Value(AbsNumber.alpha(new_length)))
                        }
                        else {
                          val __h1 = (0 until (n_len-move_start).toInt).foldLeft(h_1)((__h, i) => {
                            val s_from = AbsString.alpha((n_len -1 - i).toInt.toString)
                            val s_to = AbsString.alpha((n_len -1 -i + add_count - actualDeleteCount).toInt.toString)
                            val v = Helper.Proto(__h, l, s_from)
                            val b = Helper.HasProperty(__h, l, s_from)
                            val __h1 =
                              if (BoolTrue <= b) Helper.PropStore(__h, l, s_to, v)
                              else HeapBot
                            val __h2 =
                              if (BoolFalse <= b) Helper.Delete(__h, l, s_to)._1
                              else HeapBot
                            __h1 + __h2
                          })
                          val __h2 = (0 until add_count).foldLeft(__h1)((__h, i) =>
                            Helper.PropStore(__h, l, AbsString.alpha((actualStart + i).toInt.toString), getArgValue(__h, ctx_1, args, (i+2).toString)))
                          val new_length: Int = (n_len + add_count - actualDeleteCount).toInt
                          Helper.PropStore(__h2, l,  AbsString.alpha("length"), Value(AbsNumber.alpha(new_length)))
                        }
                      }
                      case _ => n_arglen.getAbsCase match {
                        case NumBot => HeapBot
                        case _ =>
                          val _h1 = Helper.PropStore(h_1, l, NumStr, getArgValueAbs(h_1, ctx_1, args, NumStr))
                          Helper.Delete(_h1, l, NumStr)._1
                      }
                    }
                    (_h + _h1, _o + o_1)
                  }
                  case _ => n_len.getAbsCase match {
                    case AbsBot => (HeapBot, Obj.bottom)
                    case _ =>
                      val o_new = Helper.NewArrayObject(UInt)
                      val o_1 = o_new.update(NumStr, PropValue(ObjectValue(Helper.Proto(h_1, l, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                      val _h1 = Helper.PropStore(h_1, l, NumStr, getArgValueAbs(h_1, ctx_1, args, NumStr))
                      val _h2 = Helper.Delete(_h1, l, NumStr)._1
                      (_h + _h2, _o + o_1)
                  }
                }
              })
            case _ =>
              if (n_start <= NumBot || n_count <= NumBot) {
                (HeapBot, Obj.bottom)
              }
              else {
                lset_this.foldLeft((HeapBot, Obj.bottom))((_ho, l) => {
                  val n_len = Operator.ToUInt32(Helper.Proto(h_1, l, AbsString.alpha("length")))
                  val _h = _ho._1
                  val _o = _ho._2
                  n_len match {
                    case NumBot => (HeapBot, Obj.bottom)
                    case _ =>
                      val o_new = Helper.NewArrayObject(UInt)
                      val _o1 = o_new.update(NumStr, PropValue(ObjectValue(Helper.Proto(h_1, l, NumStr),BoolTrue,BoolTrue,BoolTrue)))
                      val _h1 = Helper.Delete(h_1, l, NumStr)._1
                      val _h2 = Helper.PropStore(_h1, l, NumStr, getArgValueAbs(_h1, ctx_1, args, NumStr))
                      val _h3 = Helper.PropStore(_h2, l, AbsString.alpha("length"), Value(UInt))
                      (_h + _h3, _o + _o1)
                  }
                })
              }
          }
          if (o </ Obj.bottom) {
            val h_3 = h_2.update(l_r, o)
            ((Helper.ReturnStore(h_3, Value(l_r)), ctx_1), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      ("Array.prototype.unshift" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val (h_1, n) = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n_arglen) => {
              lset_this.foldLeft[(Heap, AbsNumber)]((h, NumBot))((_hv, l) => {
                val _h = _hv._1
                val _v = _hv._2
                if (_h <= HeapBot)
                  (HeapBot, NumBot)
                else {
                  val n_len = Operator.ToUInt32(Helper.Proto(_h, l, AbsString.alpha("length")))
                  AbsNumber.getUIntSingle(n_len) match {
                    case Some(k) => {
                      val _h1 = (0 until k.toInt).foldLeft(_h)((__h, i) => {
                        val s_from = AbsString.alpha((k - 1 - i).toInt.toString)
                        val s_to = AbsString.alpha((k -1 -i +n_arglen).toInt.toString)
                        val b = Helper.HasProperty(__h, l, s_from)
                        val __h1 =
                          if (BoolTrue <= b) Helper.PropStore(__h, l, s_to, Helper.Proto(__h, l, s_from))
                          else HeapBot
                        val __h2 =
                          if (BoolFalse <= b) Helper.Delete(__h, l, s_to)._1
                          else HeapBot
                        (__h1 + __h2)
                      })
                      val _h2 = (0 until n_arglen.toInt).foldLeft(_h1)((__h, i) => {
                        val v_i = getArgValue(h, ctx, args, i.toString)
                        Helper.PropStore(__h, l, AbsString.alpha(i.toString), v_i)
                      })
                      val _h3 = Helper.PropStore(_h2, l, AbsString.alpha("length"), Value(AbsNumber.alpha(n_arglen + k)))
                      (_h3, _v + AbsNumber.alpha(n_arglen + k))
                    }
                    case _ => n_len.getAbsCase match {
                      case AbsBot =>
                        (HeapBot, NumBot)
                      case _ =>
                        val _h1 = Helper.PropStore(_h, l, NumStr, Helper.Proto(_h, l, NumStr))
                        val _h2 = Helper.Delete(_h, l, NumStr)._1
                        (_h1 + _h2, _v + UInt)
                    }
                  }
                }
              })
            }
            case NumBot => (HeapBot, NumBot)
            case _ => {
              lset_this.foldLeft[(Heap, AbsNumber)]((h, NumBot))((_hv, l) => {
                val _h = _hv._1
                val _v = _hv._2
                if (_h <= HeapBot)
                  (HeapBot, NumBot)
                else {
                  val n_len = Operator.ToUInt32(Helper.Proto(_h, l, AbsString.alpha("length")))
                  n_len match {
                    case NumBot =>
                      (HeapBot, NumBot)
                    case _ => {
                      val _h1 = Helper.PropStore(_h, l, NumStr, Helper.Proto(_h, l, NumStr))
                      val _h2 = Helper.Delete(_h, l, NumStr)._1
                      (_h1 + _h2, _v + UInt)
                    }
                  }
                }
              })
            }
          }
          if (n </ NumBot)
            ((Helper.ReturnStore(h_1, Value(n)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      "Array.prototype.indexOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val n_index = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n) => {
              val v_search = getArgValue(h, ctx, args, "0")
              lset_this.foldLeft[AbsNumber](NumBot)((_n, l) => {
                val n_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
                _n + (AbsNumber.getUIntSingle(n_len) match {
                  case Some(n_len) => {
                    if (n_len == 0)
                      AbsNumber.alpha(-1)
                    else {
                      val start =
                        if (n > 1) Operator.ToInteger(getArgValue(h, ctx, args, "1"))
                        else AbsNumber.alpha(0)
                      AbsNumber.getUIntSingle(start) match {
                        case Some(n_start) => {
                          if (n_start >= n_len)
                            AbsNumber.alpha(-1)
                          else {
                            val k =
                              if (n_start < 0) (n_len - abs(n_start))
                              else n_start
                            val (index, flag) = (k.toInt until n_len.toInt).foldLeft[(AbsNumber, Boolean)]((NumBot, false))((__nb, i) => {
                              if (__nb._2)
                                __nb
                              else {
                                val __n = __nb._1
                                Operator.bopSEq(v_search, Helper.Proto(h, l, AbsString.alpha(i.toString))).pv._3.getPair match {
                                  case (AbsTop, _) => (__n + AbsNumber.alpha(i), false)
                                  case (AbsBot, _) => (NumBot, true)
                                  case (AbsSingle, Some(true)) => (AbsNumber.alpha(i), true)
                                  case (AbsSingle, Some(false)) => (__n, false)
                                  case _ => throw new InternalError("AbsBool does not have an abstract value for multiple values.")
                                }
                              }
                            })
                            if (flag)
                              index
                            else
                              index + AbsNumber.alpha(-1)
                          }
                        }
                        case _ => start.getAbsCase match {
                          case AbsBot => _n
                          case _ => NumTop
                        }
                      }
                    }
                  }
                  case _ => n_len.getAbsCase match {
                    case NumBot => _n
                    case _ => NumTop
                  }
                })
              })
            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => NumBot
              case _ => NumTop
            }
          }
          if (n_index </ NumBot)
            ((Helper.ReturnStore(h, Value(n_index)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "Array.prototype.lastIndexOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val n_index = AbsNumber.getUIntSingle(n_arglen) match {
            case Some(n) => {
              val v_search = getArgValue(h, ctx, args, "0")
              lset_this.foldLeft[AbsNumber](NumBot)((_n, l) => {
                val vn_len = Operator.ToUInt32(Helper.Proto(h, l, AbsString.alpha("length")))
                _n + (AbsNumber.getUIntSingle(vn_len) match {
                  case Some(n_len) if n_len == 0 => AbsNumber.alpha(-1)
                  case Some(n_len) => {
                    val start =
                      if (n > 1) Operator.ToInteger(getArgValue(h, ctx, args, "1"))
                      else AbsNumber.alpha(n_len - 1)
                    AbsNumber.getUIntSingle(start) match {
                      case Some(n_start) => {
                        val k =
                          if (n_start >= 0) min(n_start, n_len - 1)
                          else n_len - abs(n_start)
                        val (index, flag) = (0 until k.toInt).foldLeft[(AbsNumber, Boolean)]((NumBot, false))((__nb, i) => {
                          if (__nb._2)
                            __nb
                          else {
                            val __n = __nb._1
                            val i_back = (k - i).toInt
                            Operator.bopSEq(v_search, Helper.Proto(h, l, AbsString.alpha(i_back.toString))).pv._3.getPair match {
                              case (AbsTop, _) => (__n + AbsNumber.alpha(i_back), false)
                              case (AbsBot, _) => (NumBot, true)
                              case (AbsSingle, Some(true)) => (AbsNumber.alpha(i_back), true)
                              case (AbsSingle, Some(false)) => (__n, false)
                              case _ => throw new InternalError("AbsBool does not have an abstract value for multiple values.")
                            }
                          }
                        })
                        if (flag)
                          index
                        else
                          index + AbsNumber.alpha(-1)
                      }
                      case _ => start.getAbsCase match {
                        case AbsBot => _n
                        case _ => NumTop
                      }
                    }
                  }
                  case _ => vn_len.getAbsCase match {
                    case AbsBot => _n
                    case _ => NumTop
                  }
                })
              })
            }
            case _ => n_arglen.getAbsCase match {
              case AbsBot => NumBot
              case _ => NumTop
            }
          }
          if (n_index </ NumBot)
            ((Helper.ReturnStore(h, Value(n_index)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "Array.prototype.forEach.init" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_this = h(SinglePureLocalLoc)("@this")._2
          val v_callbackfn = getArgValue(h, ctx, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val v_thisArg =
            n_arglen.getSingle match {
              case Some(d) if d >= 2 => Some(getArgValue(h, ctx, args, "1"))
              case None if AbsNumber.alpha(2) <= n_arglen => Some(getArgValue(h, ctx, args, "1"))
              case None => None
              case _ => None
            }
          val bthisArgValue = v_thisArg.isDefined

          // Get a new address
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)

          // 1. Let O be the result of calling ToObject passing the this value as the argument.
          val (v_this2, h_1, ctx_1, es_1) = Helper.toObject(h, ctx, v_this, addr1)
          val lset_this = v_this2.locs


          // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
          val es_2 =
            if (BoolFalse <= Helper.IsCallable(h_1, v_callbackfn))
              Set[Exception](TypeError)
            else
              ExceptionBot

          val es = es_1 ++ es_2 
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)

          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        }),
      "Array.prototype.forEach.call" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_this = h(SinglePureLocalLoc)("@this")._2

          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)

          val v_callbackfn = getArgValue(h, ctx, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          
          val v_thisArg =
            n_arglen.getSingle match {
              case Some(d) if d >= 2 => Some(getArgValue(h, ctx, args, "1"))
              case None if AbsNumber.alpha(2) <= n_arglen => Some(getArgValue(h, ctx, args, "1"))
              case None => None
              case _ => None
            }
          
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val addr2 = cfg.getAPIAddress(addr_env, 1)
          val addr3 = cfg.getAPIAddress(addr_env, 2)
          val l_r1 = addrToLoc(addr1, Recent)
          val l_r2 = addrToLoc(addr2, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2)

          val (v_this2, h_3, ctx_3, es_1) = Helper.toObject(h_2, ctx_2, v_this, addr3)

          val cond = v_callbackfn.locs.exists((l) => BoolFalse <= Helper.IsCallable(h_3, l))
          val es =
            if (cond) Set[Exception](TypeError)
            else Set[Exception]()
          val (h_e, ctx_e) = Helper.RaiseException(h_3, ctx_3, es)
          val lset_f = v_callbackfn.locs.filter((l) => BoolTrue <= Helper.IsCallable(h_3, l))
          val lset_this = v_this2.locs

          val value = lset_this.foldLeft(ValueBot)((v, l) => v + Helper.Proto(h_3, l, absNumberToString(AbsNumber.naturalNumbers)))
      
          // create Arguments object
          val o_arg =
            Helper.NewArgObject(AbsNumber.alpha(3))
              .update(AbsString.alpha("0"), PropValue(ObjectValue(value, T, T, T))) // kValue
              .update(AbsString.alpha("1"), PropValue(ObjectValue(Value(AbsNumber.naturalNumbers), T, T, T))) // k
              .update(AbsString.alpha("2"), PropValue(ObjectValue(v_this2, T, T, T))) // O

          val h_4 = h_3.update(l_r1, o_arg)
          val v_arg = Value(l_r1)

          // 5. If thisArg wes supplied, let T be thisArg; else let T be undefined
          val callee_this = v_thisArg match {
              case Some(v) => v
              case None => Value(GlobalSingleton)
           }
          val o_old = h_4(SinglePureLocalLoc)
          val cc_caller = cp._2
          val n_aftercall = cfg.getAftercallFromCall(cp._1)
          val cp_aftercall = (n_aftercall, cc_caller)
          val n_aftercatch = cfg.getAftercatchFromCall(cp._1)
          val cp_aftercatch = (n_aftercatch, cc_caller)
          lset_f.foreach((l_f) => {
            val o_f = h_4(l_f)
            o_f("@function")._3.foreach((fid) => {
              cc_caller.NewCallContext(h, cfg, fid, l_r2, callee_this.locs).foreach((pair) => {
                val (cc_new, o_new) = pair
                val o_new2 = o_new.
                  update(cfg.getArgumentsName(fid),
                  PropValue(ObjectValue(v_arg, BoolTrue, BoolFalse, BoolFalse))).
                  update("@scope", o_f("@scope"))
                sem.addCallEdge(cp, ((fid, LEntry), cc_new), ContextEmpty, o_new2)
                sem.addReturnEdge(((fid, LExit), cc_new), cp_aftercall, ctx_3, o_old)
                sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_3, o_old)
              })
            })
          })
          val h_5 = v_arg.locs.foldLeft(HeapBot)((hh, l) => {
            hh + h_4.update(l, h_4(l).update("callee",
              PropValue(ObjectValue(Value(lset_f), BoolTrue, BoolFalse, BoolTrue))))
          })

          val s_1 = (he + h_e, ctxe + ctx_e)
          
          ((h_5, ctx_3), s_1)
        }),

      "Array.prototype.forEach.ret" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 8. Return undefined
          val h_1 = Helper.PropStore(h, SinglePureLocalLoc, AbsString.alpha("temp"), Value(UndefTop))
          ((h_1, ctx), (he, ctxe))
        }),

      "Array.prototype.reduce.init" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_this = h(SinglePureLocalLoc)("@this")._2
          val v_callbackfn = getArgValue(h, ctx, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val v_initial =
            n_arglen.getSingle match {
              case Some(d) if d >= 2 => Some(getArgValue(h, ctx, args, "1"))
              case None if AbsNumber.alpha(2) <= n_arglen => Some(getArgValue(h, ctx, args, "1"))
              case None => None
              case _ => None
            }
          val bInitValue = v_initial.isDefined

          // Get a new address
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)

          // 1. Let O be the result of calling ToObject passing the this value as the argument.
          val (v_this2, h_1, ctx_1, es_1) = Helper.toObject(h, ctx, v_this, addr1)
          val lset_this = v_this2.locs

          // 2. Let lenValue be the result of calling the [[Get]] internal method of O with the argument "length".
          val v_len = lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, AbsString.alpha("length")))
          // 3. Let len be ToUint32(lenValue).
          val n_len = Operator.ToUInt32(v_len)

          // 4. If IsCallable(callbackfn) is false, throw a TypeError exception.
          val es_2 =
            if (BoolFalse <= Helper.IsCallable(h_1, v_callbackfn))
              Set[Exception](TypeError)
            else
              ExceptionBot

          // 5. If len is 0 and initialValue is not present, throw a TypeError exception.
          val es_3 =
            if ((AbsNumber.alpha(0) <= n_len) && !bInitValue)
              Set[Exception](TypeError)
            else
              ExceptionBot

          val h_2 = if(bInitValue) Helper.PropStore(h, SinglePureLocalLoc, AbsString.alpha("temp"), v_initial.get)
                    else h_1

          // If initialValue is not present
          // c. If kPresent is false, throw a TypeError exception.
          val es_4 =
            n_len.getSingle match {
              case Some(len) if !bInitValue => {
                val present =
                  (0 to (len.toInt-1)).foldLeft(false)((b, k) => {
                    if (b) b
                    else {
                      val v = lset_this.foldLeft(ValueBot)((_v, l) => _v + Helper.Proto(h_1, l, AbsString.alpha(k.toString)))
                      val v_1 = Value(PValue(UndefBot, v.pv._2, v.pv._3, v.pv._4, v.pv._5), v.locs)
                      v_1 </ ValueBot
                    }
                  })
                if (!present) Set[Exception](TypeError)
                else ExceptionBot
              }
              case _ => Set[Exception](TypeError)
            }

          val es = es_1 ++ es_2 ++ es_3 ++ es_4
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)

          ((h_2, ctx_1), (he + h_e, ctxe + ctx_e))
        }),
      "Array.prototype.reduce.call" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_this = h(SinglePureLocalLoc)("@this")._2

          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)

          val v_callbackfn = getArgValue(h, ctx, args, "0")
          val n_arglen = Operator.ToUInt32(getArgValue(h, ctx, args, "length"))
          val v_initial =
            n_arglen.getSingle match {
              case Some(d) if d >= 2 => Some(getArgValue(h, ctx, args, "1"))
              case None if AbsNumber.alpha(2) <= n_arglen => Some(getArgValue(h, ctx, args, "1"))
              case None => None
              case _ => None
            }

          val addr1 = cfg.getAPIAddress(addr_env, 1)
          val addr2 = cfg.getAPIAddress(addr_env, 2)
          val addr3 = cfg.getAPIAddress(addr_env, 0)
          val addr4 = cfg.getAPIAddress(addr_env, 3)
          val l_r1 = addrToLoc(addr1, Recent)
          val l_r2 = addrToLoc(addr2, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2)

          val (v_this2, h_3, ctx_3, es_1) = Helper.toObject(h_2, ctx_2, v_this, addr3)

          // 1.
          val cond = v_callbackfn.locs.exists((l) => BoolFalse <= Helper.IsCallable(h_3, l))
          val es =
            if (cond) Set[Exception](TypeError)
            else Set[Exception]()
          val (h_e, ctx_e) = Helper.RaiseException(h_3, ctx_3, es)
          val lset_f = v_callbackfn.locs.filter((l) => BoolTrue <= Helper.IsCallable(h_3, l))
          val lset_tarf = v_callbackfn.locs.filter(l => BoolTrue <= Helper.IsBound(h_3,l))
          val lset_this = v_this2.locs

          val value = lset_this.foldLeft(ValueBot)((v, l) => v + Helper.Proto(h_3, l, absNumberToString(AbsNumber.naturalNumbers)))
          val temp = h_3(SinglePureLocalLoc)("temp")._1._1
          val temp_2 = v_initial match {
            case Some(v) => v + temp + value
            case None => temp + value
          }

          // 2., 3. create Arguments object
          val o_arg =
            Helper.NewArgObject(AbsNumber.alpha(4))
              .update(AbsString.alpha("0"), PropValue(ObjectValue(temp_2, T, T, T))) // accumulator
              .update(AbsString.alpha("1"), PropValue(ObjectValue(value, T, T, T))) // kValue
              .update(AbsString.alpha("2"), PropValue(ObjectValue(Value(AbsNumber.naturalNumbers), T, T, T))) // k
              .update(AbsString.alpha("3"), PropValue(ObjectValue(v_this2, T, T, T))) // O
           //////////////////////////////////////////////////
          // check whether there is a bound function or not
          val o_arg_1 =
            if(lset_tarf.isEmpty) {
              o_arg
            } else {
              val lset_target_args = lset_tarf.foldLeft(LBot)((lset, l_tf) => {
                h_3(l_tf)("@bound_args")._2.locs ++ lset
              })
              val target_args = lset_target_args.foldLeft(Obj.bottom)((obj, l_ta) => obj + h_3(l_ta))
              Helper.concat(target_args, o_arg)
            }

          val h_4 = h_3.update(l_r1, o_arg_1)
          val v_arg = Value(l_r1)

          val callee_this = Value(GlobalSingleton)

          val o_old = h_4(SinglePureLocalLoc)
          val cc_caller = cp._2
          val n_aftercall = cfg.getAftercallFromCall(cp._1)
          val cp_aftercall = (n_aftercall, cc_caller)
          val n_aftercatch = cfg.getAftercatchFromCall(cp._1)
          val cp_aftercatch = (n_aftercatch, cc_caller)
         
          lset_tarf.foreach((l_f) => {
            val o_f = h_4(l_f)
            val l_this = o_f("@bound_this")._2.locs
            //o_f("@target_function")._1._3.foreach((fid) => {
            val (fids, scope_locs) = o_f("@target_function")._2.locs.foldLeft((FunSetBot, LocSetBot))((fidslocs, l) => ((fidslocs._1 ++ h_4(l)("@function")._3, fidslocs._2 ++ h_4(l)("@scope")._2.locs)))
            fids.foreach((fid) => {
              cc_caller.NewCallContext(h, cfg, fid, l_r2, l_this).foreach((pair) => {
                val (cc_new, o_new) = pair
                val o_new2 = o_new.
                  update(cfg.getArgumentsName(fid),
                  PropValue(ObjectValue(v_arg, BoolTrue, BoolFalse, BoolFalse))).
               //   update("@scope", o_f("@scope"))
                  update("@scope", PropValue(ObjectValue(Value(scope_locs), BoolBot, BoolBot, BoolBot), FunSetBot))
                sem.addCallEdge(cp, ((fid,LEntry), cc_new), ContextEmpty, o_new2)
                sem.addReturnEdge(((fid,LExit), cc_new), cp_aftercall, ctx_3, o_old)
                sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_3, o_old)
              })
            })
          })


          lset_f.foreach((l_f) => {
            val o_f = h_4(l_f)
            o_f("@function")._3.foreach((fid) => {
              cc_caller.NewCallContext(h, cfg, fid, l_r2, callee_this.locs).foreach((pair) => {
                val (cc_new, o_new) = pair
                val o_new2 = o_new.
                  update(cfg.getArgumentsName(fid),
                  PropValue(ObjectValue(v_arg, BoolTrue, BoolFalse, BoolFalse))).
                  update("@scope", o_f("@scope"))
                sem.addCallEdge(cp, ((fid, LEntry), cc_new), ContextEmpty, o_new2)
                sem.addReturnEdge(((fid, LExit), cc_new), cp_aftercall, ctx_3, o_old)
                sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_3, o_old)
              })
            })
          })
          val h_5 = v_arg.locs.foldLeft(HeapBot)((hh, l) => {
            hh + h_4.update(l, h_4(l).update("callee",
              PropValue(ObjectValue(Value(lset_f), BoolTrue, BoolFalse, BoolTrue))))
          })

          val s_1 = (he + h_e, ctxe + ctx_e)
          ((h_5, ctx_3), s_1)
        }),
      "Array.prototype.reduce.ret" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((h, ctx), (he, ctxe))
        })
    )

  }


}
