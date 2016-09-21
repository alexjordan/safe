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

import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, InternalError, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.bug_detector.{RegExp4_1_1, RegExp4_1_2, RegExp2_5, RegExp2_15_2}
import kr.ac.kaist.jsaf.utils.regexp.{JSRegExpSolver, ESyntax, ERegExp4_1_1, ERegExp4_1_2, ERegExp2_5, ERegExp2_15_2, SyntaxErrorException}
import scala.collection.immutable.HashSet
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell

object BuiltinRegExp extends ModelData {

  val ConstLoc = newSystemLoc("RegExpConst", Recent)
  val ProtoLoc = newSystemLoc("RegExpProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("RegExp")),
    ("@construct",               AbsInternalFunc("RegExp.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(ProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(2), F, F, F)))),
    ("$1",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$2",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$3",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$4",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$5",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$6",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$7",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$8",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("$9",                       AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F))))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("RegExp")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(BoolTrue))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(ConstLoc, F, F, F)))),
    ("exec",                 AbsBuiltinFunc("RegExp.prototype.exec", 1)),
    ("test",                 AbsBuiltinFunc("RegExp.prototype.test", 1)),
    ("toString",             AbsBuiltinFunc("RegExp.prototype.toString", 0))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ProtoLoc, prop_proto)
  )

  // lset_this : location set of regular expression objects
  // argVal: argument string
  // l_r: location of the return object
  def exec(h: Heap, ctx: Context, he: Heap, ctxe: Context, lset_this: LocSet, argVal: AbsString, l_r: Loc, addr1: Address): ((Heap, Context), (Heap, Context)) = {
    val lset_1 = lset_this.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2.pv._5)
    val lset_2 = lset_this.filter(l => {
      val s = h(l)("@class")._2.pv._5
      AbsString.alpha("RegExp") != s && s </ StrBot
    })

    // if 'this' object is an object whose [[class]] is a 'RegExp'
    val (h_4, ctx_4) =
      if (!lset_1.isEmpty) {
        val l = lset_1.head
        val src = h(l)("source")._1._1.pv._5.getSingle
        val b_g = h(l)("global")._1._1.pv._3.getSingle
        val b_i = h(l)("ignoreCase")._1._1.pv._3.getSingle
        val b_m = h(l)("multiline")._1._1.pv._3.getSingle
        val idx = Operator.ToInteger(h(l)("lastIndex")._1._1).getSingle
        val s_1 = argVal.gamma

        val (h_3, ctx_3) = (lset_1.size, src, b_g, b_i, b_m, idx, s_1) match {
          case (1, Some(source), Some(g), Some(i), Some(m), Some(lastIdx), Some(argset)) => {
            val flags = (if (g) "g" else "") + (if (i) "i" else "") + (if (m) "m" else "")

            val (matcher, _, _, _, _) = JSRegExpSolver.parse(source, flags)

            val lastIdx_ : Int = if (g) lastIdx.toInt else 0

            argset.foldLeft((HeapBot, ContextBot))((hc, arg) => {
              val (array, lastIndex, index, length) = JSRegExpSolver.exec(matcher, arg, lastIdx_)

              // XXX: Need to check the semantics of [[Put]] internal method.
              val h_1 =
                if (g) Helper.PropStore(h, l, AbsString.alpha("lastIndex"), Value(AbsNumber.alpha(lastIndex)))
                else h

              val (h_2, ctx_1) = array match {
                case Some(_) => Helper.Oldify(h_1, ctx, addr1)
                case None => (h_1, ctx)
              }

              val (h_4, ctx_4) = array match {
                case Some(arr) => {
                  val newobj = Helper.NewArrayObject(AbsNumber.alpha(length))
                    .update("index", PropValue(ObjectValue(AbsNumber.alpha(index), T, T, T)))
                    .update("input", PropValue(ObjectValue(argVal, T, T, T)))

                  val newobj_1 = (0 to length - 1).foldLeft(newobj)((no, i) => {
                    val v = arr(i) match {
                      case Some(s) => Value(AbsString.alpha(s))
                      case None => Value(UndefTop)
                    }
                    no.update(AbsString.alpha(i.toString), PropValue(ObjectValue(v, T, T, T)))
                  })
                  val h_3 = h_2.update(l_r, newobj_1)
                  (Helper.ReturnStore(h_3, Value(l_r)), ctx_1)
                }
                case None => {
                  (Helper.ReturnStore(h_2, Value(NullTop)), ctx_1)
                }
              }
              (hc._1 + h_4, hc._2 + ctx_4)
            })
          }
          case _ => {
            // argument value
            if (argVal </ StrBot) {
              val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
              val newobj = Helper.NewArrayObject(UInt)
                .update("index", PropValue(ObjectValue(UInt, T, T, T)))
                .update("input", PropValue(ObjectValue(argVal, T, T, T)))
                .update(Str_default_number, PropValue(ObjectValue(Value(StrTop) + Value(UndefTop), T, T, T)))

              val h_2 = lset_1.foldLeft(h_1)((h_1_, l) => Helper.PropStore(h_1_, l, AbsString.alpha("lastIndex"), Value(UInt)))
              val h_3 = h_2.update(l_r, newobj)
              (Helper.ReturnStore(h_3, Value(l_r) + Value(NullTop)), ctx_1)
            } else {
              (HeapBot, ContextBot)
            }
          }
        }
        (h_3, ctx_3)
      } else {
        (HeapBot, ContextBot)
      }


    // if 'this' object is not an object whose [[class]] is a 'RegExp', throw a TypeError exception.
    val es =
      if (!lset_2.isEmpty) HashSet[Exception](TypeError)
      else ExceptionBot
    val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)

    ((h_4, ctx_4), (he + he_1, ctxe + ctxe_1))


  }

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      "RegExp" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // API address allocation
          val lset_callee = getArgValue(h, ctx, args, "callee").locs
          val abstraction = lset_callee.size > 1
          
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)

          val v_1 = getArgValue(h, ctx, args, "0")
          val v_2 = getArgValue(h, ctx, args, "1")

          // case for pattern is undefined.
          val p_1 =
            if (v_1.pv._1 </ UndefBot) AbsString.alpha("")
            else StrBot
          // case for flags is undefined.
          val f_1 =
            if (v_2.pv._1 </ UndefBot) AbsString.alpha("")
            else StrBot

          // case for pattern is an object whose [[class]] is RegExp.
          val lset_1 = v_1.locs.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2.pv._5)
          // case for pattern is an object whose [[class]] is not a RegExp.
          val lset_2 = v_1.locs.filter(l => {
            val s = h(l)("@class")._2.pv._5
            AbsString.alpha("RegExp") != s && s </ StrBot
          })

          // case for pattern is a value which is not an undefined or an object whose [[class] is not a RegExp.
          val v_1_ = Value(PValue(UndefBot, v_1.pv._2, v_1.pv._3, v_1.pv._4, v_1.pv._5), lset_2)
          val p_3 = Helper.toString(Helper.toPrimitive_better(h, v_1_))
          // case for flags is a value which is not an undefined or an object.
          val v_2_ = Value(PValue(UndefBot, v_2.pv._2, v_2.pv._3, v_2.pv._4, v_2.pv._5), v_2.locs)
          val f_2 = Helper.toString(Helper.toPrimitive_better(h, v_2_))

          // If pattern is an object R whose [[Class] internal property is "RegExp" and
          // flags is not undefined, then throw a "TypeError" exception.
          val es_1 = if (!lset_1.isEmpty && !(v_2_ <= ValueBot)) {
            HashSet[Exception](TypeError)
          } else {
            ExceptionBot
          }

          // case for pattern is a value or an object whose [[class]] is not a RegExp.
          val p = p_1 + p_3
          // case for flags is a value or an object.
          val f = f_1 + f_2

          val (oo, es_2) = (p.gamma, f.gamma) match {
            case (Some(patternSet), Some(flagsSet)) =>
              var obj: Obj = Obj.bottom
              var exc = ExceptionBot
              for(pattern <- patternSet) for(flags <- flagsSet) {
                val s =
                  if (pattern == "") "(?:)"
                  else pattern

                try {
                  val (_, b_g, b_i, b_m, _) = JSRegExpSolver.parse(s, flags)

                  obj += Helper.NewRegExp(AbsString.alpha(s), AbsBool.alpha(b_g), AbsBool.alpha(b_i), AbsBool.alpha(b_m))
                } catch {
                  case e: SyntaxErrorException =>
                    if (Config.typingInterface != null)
                      if (Shell.params.opt_DeveloperMode || !abstraction)
                        e.getKind match {
                          case ESyntax => ()
                          case ERegExp4_1_1 => Config.typingInterface.signal(null, RegExp4_1_1, e.getMsg1, e.getMsg2)
                          case ERegExp4_1_2 => Config.typingInterface.signal(null, RegExp4_1_2, e.getMsg1, e.getMsg2)
                          case ERegExp2_5 => Config.typingInterface.signal(null, RegExp2_5, e.getMsg1, e.getMsg2)
                          case ERegExp2_15_2 => Config.typingInterface.signal(null, RegExp2_15_2, e.getMsg1, e.getMsg2)
                        }
                    exc = HashSet[Exception](SyntaxError)
                  case e: InternalError => throw e
                }
              }
              (Some(obj), exc)

            case _ if p </ StrBot && f </ StrBot => (Some(Helper.NewRegExp(p, BoolTop, BoolTop, BoolTop)), HashSet[Exception](SyntaxError))
            case _ => (None, ExceptionBot)
          }

          val (h_1, ctx_1) = oo match {
            case Some(o) => {
              val (h_1_, ctx_1_) = Helper.Oldify(h, ctx, addr1)
              val h_2_ = h_1_.update(l_r, o)
              (h_2_, ctx_1_)
            }
            case None => (h, ctx)
          }

          val v_rtn_1 = oo match {
            case Some(o) => Value(l_r)
            case None => ValueBot
          }
          val v_rtn_2 =
            if ((!lset_1.isEmpty) && v_2.pv._1 </ UndefBot) Value(lset_1)
            else ValueBot

          val v_rtn = v_rtn_1 + v_rtn_2

          val (h_2, ctx_2) =
            if (v_rtn </ ValueBot) (Helper.ReturnStore(h_1, v_rtn), ctx_1)
            else (HeapBot, ContextBot)

          val es = es_1 ++ es_2
          val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)
          ((h_2, ctx_2), (he + he_1, ctxe + ctxe_1))
        }),
      "RegExp.constructor" -> (
      (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
        val lset_callee = getArgValue(h, ctx, args, "callee").locs
        val abstraction = lset_callee.size > 1
        val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
        val v_1 = getArgValue(h, ctx, args, "0")
        val v_2 = getArgValue(h, ctx, args, "1")

        // case for pattern is undefined.
        val p_1 =
          if (v_1.pv._1 </ UndefBot) AbsString.alpha("")
          else StrBot
        // case for flags is undefined.
        val f_1 =
          if (v_2.pv._1 </ UndefBot) AbsString.alpha("")
          else StrBot

        // case for pattern is an object whose [[class]] is RegExp.
        val lset_1 = v_1.locs.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2.pv._5)
        val p_2 = lset_1.foldLeft[AbsString](StrBot)((s, l) => s + h(l)("source")._1._1.pv._5)

        // case for pattern is an object whose [[class]] is not a RegExp.
        val lset_2 = v_1.locs.filter(l => {
          val s = h(l)("@class")._2.pv._5
          AbsString.alpha("RegExp") != s && s </ StrBot
        })

        // case for pattern is a value which is not an undefined or an object whose [[class] is not a RegExp.
        val v_1_ = Value(PValue(UndefBot, v_1.pv._2, v_1.pv._3, v_1.pv._4, v_1.pv._5), lset_2)
        val p_3 = Helper.toString(Helper.toPrimitive_better(h, v_1_))
        // case for flags is a value which is not an undefined or an object.
        val v_2_ = Value(PValue(UndefBot, v_2.pv._2, v_2.pv._3, v_2.pv._4, v_2.pv._5), v_2.locs)
        val f_2 = Helper.toString(Helper.toPrimitive_better(h, v_2_))

        // If pattern is an object R whose [[Class] internal property is "RegExp" and
        // flags is not undefined, then throw a "TypeError" exception.
        val es_1 = if (!lset_1.isEmpty && !(v_2_ <= ValueBot)) {
          HashSet[Exception](TypeError)
        } else {
          ExceptionBot
        }

        // case for pattern is a value or an object
        val p = p_1 + p_2 + p_3
        // case for flags is a value or an object.
        val f = f_1 + f_2

        val (oo, es_2) = (p.gamma, f.gamma) match {
          case (Some(patternSet), Some(flagsSet)) =>
            try {
              var obj: Obj = Obj.bottom
              for(pattern <- patternSet) for(flags <- flagsSet) {
                val s =
                  if (pattern == "") "(?:)"
                  else pattern
                val (_, b_g, b_i, b_m, _) = JSRegExpSolver.parse(s, flags)

                obj+= Helper.NewRegExp(AbsString.alpha(s), AbsBool.alpha(b_g), AbsBool.alpha(b_i), AbsBool.alpha(b_m))
              }
              (Some(obj), ExceptionBot)
            } catch {
              case e: SyntaxErrorException =>
                if (Config.typingInterface != null)
                  if (Shell.params.opt_DeveloperMode || !abstraction)
                    e.getKind match {
                      case ESyntax => ()
                      case ERegExp4_1_1 => Config.typingInterface.signal(null, RegExp4_1_1, e.getMsg1, e.getMsg2)
                      case ERegExp4_1_2 => Config.typingInterface.signal(null, RegExp4_1_2, e.getMsg1, e.getMsg2)
                      case ERegExp2_5 => Config.typingInterface.signal(null, RegExp2_5, e.getMsg1, e.getMsg2)
                      case ERegExp2_15_2 => Config.typingInterface.signal(null, RegExp2_15_2, e.getMsg1, e.getMsg2)
                    }
                (None, HashSet[Exception](SyntaxError))
              case _: Throwable => (None, HashSet[Exception](SyntaxError))
            }
          case _ if p </ StrBot && f </ StrBot => (Some(Helper.NewRegExp(p, BoolTop, BoolTop, BoolTop)), HashSet[Exception](SyntaxError))
          case _ => (None, ExceptionBot)
        }

        val (h_1, ctx_1) = oo match {
          case Some(o) => {
            val h_1_ = lset_this.foldLeft(h)((h_, l) => h_.update(l, o))
            (h_1_, ctx)
          }
          case _ => (HeapBot, ContextBot)
        }

        val es = es_1 ++ es_2
        val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)
        ((h_1, ctx_1), (he + he_1, ctxe + ctxe_1))
      }),
      // imprecise semantics
      "RegExp.prototype.exec" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // allocate new location
          val v_1 = getArgValue(h, ctx, args, "0")
          val argVal = Helper.toString(Helper.toPrimitive_better(h, v_1))
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          
          exec(h, ctx, he, ctxe, lset_this, argVal, l_r, addr1)
          /*
          val lset_1 = lset_this.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2._1._5)
          val lset_2 = lset_this.filter(l => {
            val s = h(l)("@class")._2._1._5
            AbsString.alpha("RegExp") != s && s </ StrBot
          })

          // if 'this' object is an object whose [[class]] is a 'RegExp'
          val (h_4, ctx_4) =
            if (!lset_1.isEmpty) {
              val l = lset_1.head
              val src = h(l)("source")._1._1._1._5.getSingle
              val b_g = h(l)("global")._1._1._1._3.getSingle
              val b_i = h(l)("ignoreCase")._1._1._1._3.getSingle
              val b_m = h(l)("multiline")._1._1._1._3.getSingle
              val idx = Operator.ToInteger(h(l)("lastIndex")._1._1._1).getSingle
              val s_1 = argVal.getSingle

              val (h_3, ctx_3) = (lset_1.size, src, b_g, b_i, b_m, idx, s_1) match {
                case (1, Some(source), Some(g), Some(i), Some(m), Some(lastIdx), Some(arg)) => {
                  val flags = (if (g) "g" else "") + (if (i) "i" else "") + (if (m) "m" else "")

                  val (matcher, _, _, _, _) = JSRegExpSolver.parse(source, flags)

                  val lastIdx_ : Int = if (g) lastIdx.toInt else 0
                  val (array, lastIndex, index, length) = JSRegExpSolver.exec(matcher, arg, lastIdx_)

                  // XXX: Need to check the semantics of [[Put]] internal method.
                  val h_1 =
                    if (g) Helper.PropStore(h, l, AbsString.alpha("lastIndex"), Value(AbsNumber.alpha(lastIndex)))
                    else h

                  val (h_2, ctx_1) = array match {
                    case Some(_) => Helper.Oldify(h_1, ctx, addr1)
                    case None => (h_1, ctx)
                  }

                  array match {
                    case Some(arr) => {
                      val newobj = Helper.NewArrayObject(AbsNumber.alpha(length))
                        .update("index", PropValue(ObjectValue(AbsNumber.alpha(index), T, T, T)))
                        .update("input", PropValue(ObjectValue(argVal, T, T, T)))

                      val newobj_1 = (0 to length - 1).foldLeft(newobj)((no, i) => {
                        val v = arr(i) match {
                          case Some(s) => Value(AbsString.alpha(s))
                          case None => Value(UndefTop)
                        }
                        no.update(AbsString.alpha(i.toString), PropValue(ObjectValue(v, T, T, T)))
                      })
                      val h_3 = h_2.update(l_r, newobj_1)
                      (Helper.ReturnStore(h_3, Value(l_r)), ctx_1)
                    }
                    case None => {
                      (Helper.ReturnStore(h_2, Value(NullTop)), ctx_1)
                    }
                  }
                }
                case _ => {
                  // argument value
                  if (argVal </ StrBot) {
                    val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
                    val newobj = Helper.NewArrayObject(UInt)
                      .update("index", PropValue(ObjectValue(UInt, T, T, T)))
                      .update("input", PropValue(ObjectValue(argVal, T, T, T)))
                      .update(Str_default_number, PropValue(ObjectValue(Value(StrTop) + Value(UndefTop), T, T, T)))

                    val h_2 = lset_1.foldLeft(h_1)((h_1_, l) => Helper.PropStore(h_1_, l, AbsString.alpha("lastIndex"), Value(UInt)))
                    val h_3 = h_2.update(l_r, newobj)
                    (Helper.ReturnStore(h_3, Value(l_r) + Value(NullTop)), ctx_1)
                  } else {
                    (HeapBot, ContextBot)
                  }
                }
              }
              (h_3, ctx_3)
            } else {
              (HeapBot, ContextBot)
            }


          // if 'this' object is not an object whose [[class]] is a 'RegExp', throw a TypeError exception.
          val es =
            if (!lset_2.isEmpty) HashSet[Exception](TypeError)
            else ExceptionBot
          val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)

          ((h_4, ctx_4), (he + he_1, ctxe + ctxe_1)) */
        }),
      "RegExp.prototype.test" -> (
       (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
         val v_1 = getArgValue(h, ctx, args, "0")
         val argVal = Helper.toString(Helper.toPrimitive_better(h, v_1))
         val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
         val lset_1 = lset_this.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2.pv._5)
         val lset_2 = lset_this.filter(l => {
           val cls = h(l)("@class")._2.pv._5
           AbsString.alpha("RegExp") != cls && cls </ StrBot
         })

         // if 'this' object is an object whose [[class]] is a 'RegExp'
         val (h_4, ctx_4) =
           if (!lset_1.isEmpty) {
             val l = lset_1.head
             val a_src = h(l)("source")._1._1.pv._5
             val a_g = h(l)("global")._1._1.pv._3
             val a_i = h(l)("ignoreCase")._1._1.pv._3
             val a_m = h(l)("multiline")._1._1.pv._3
             val a_idx = h(l)("lastIndex")._1._1
             val src = a_src.getSingle
             val b_g = a_g.getSingle
             val b_i = a_i.getSingle
             val b_m = a_m.getSingle
             val idx = Operator.ToInteger(a_idx).getSingle
             val s_1 = argVal.getSingle

             val (a_lastidx, b_rtn) = (lset_1.size, src, b_g, b_i, b_m, idx, s_1) match {
               // case for a concrete input.
               case (1, Some(source), Some(g), Some(i), Some(m), Some(lastIdx), Some(arg)) => {
                 val flags = (if (g) "g" else "") + (if (i) "i" else "") + (if (m) "m" else "")

                 val (matcher, _, _, _, _) = JSRegExpSolver.parse(source, flags)

                 val lastIdx_ : Int = if (g) lastIdx.toInt else 0
                 val (array, lastIndex, _, _) = JSRegExpSolver.exec(matcher, arg, lastIdx_)

                 val absLastIndex = AbsNumber.alpha(lastIndex)

                 val b_rtn_ = array match {
                   case Some(_) => BoolTrue
                   case _ => BoolFalse
                 }
                 (absLastIndex, b_rtn_)
               }
               // case for an abstract input which is not a bottom.
               case _ if a_src </ StrBot && a_g </ BoolBot && a_i </ BoolBot && a_m </ BoolBot && a_idx </ ValueBot && argVal </ StrBot => (UInt, BoolTop)
               // otherwise.
               case _ => (NumBot, BoolBot)
             }

             val a_g_ = lset_1.foldLeft(AbsBool.bot)((b, l) => b + h(l)("global")._1._1.pv._3)

             // XXX: Need to check the semantics of [[Put]] internal method.
             val h_2 =
               if (BoolTrue <= a_g_) lset_1.foldLeft(h)((h_, l) => Helper.PropStore(h_, l, AbsString.alpha("lastIndex"), Value(a_lastidx)))
               else h

             val (h_3, ctx_3) =
               if (b_rtn </ BoolBot) {
                 (Helper.ReturnStore(h_2, Value(b_rtn)), ctx)
               } else {
                 (HeapBot, ContextBot)
               }

             (h_3, ctx_3)
           } else {
             (HeapBot, ContextBot)
           }

         val es =
           if (!lset_2.isEmpty) HashSet[Exception](TypeError)
           else ExceptionBot
         val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)

         ((h_4, ctx_4), (he + he_1, ctxe + ctxe_1))
       }),
      "RegExp.prototype.toString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs

          val lset_1 = lset_this.filter(l => AbsString.alpha("RegExp") <= h(l)("@class")._2.pv._5)
          val lset_2 = lset_this.filter(l => AbsString.alpha("RegExp") </ h(l)("@class")._2.pv._5)

          val s_rtn = Helper.defaultToString(h, lset_1)

          val (h_1, ctx_1) =
            if (s_rtn </ StrBot) (Helper.ReturnStore(h, Value(s_rtn)), ctx)
            else (HeapBot, ContextBot)

          val es =
            if (!lset_2.isEmpty) HashSet[Exception](TypeError)
            else ExceptionBot
          val (he_1, ctxe_1) = Helper.RaiseException(h, ctx, es)

          ((h_1, ctx_1), (he + he_1, ctxe + ctxe_1))
        })
    )
  }

}
