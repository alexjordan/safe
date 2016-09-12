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

import kr.ac.kaist.jsaf.analysis.cfg.{CFG, CFGExpr, LEntry, LExit, LExitExc, InternalError, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.ShellParameters

object BuiltinFunction extends ModelData {

  val ConstLoc = newSystemLoc("FunctionConst", Recent)
  //val ProtoLoc = newPreDefLoc("FunctionProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(LocSet(FunctionProtoLoc)), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Function.constructor")),
    ("@construct",               AbsInternalFunc("Function.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F))))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(BoolTrue))),
    ("@function",            AbsInternalFunc("Function.prototype")),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(ConstLoc, T, F, T)))),
    ("length",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0), F, F, F)))),
    ("toString",             AbsBuiltinFunc("Function.prototype.toString", 0)),
    ("apply",                AbsBuiltinFuncAftercall("Function.prototype.apply", 2)),
    ("call",                 AbsBuiltinFuncAftercall("Function.prototype.call", 1)),
    // ("bind",                 AbsBuiltinFuncAftercall("Function.prototype.bind", 1))
    ("bind",                 AbsBuiltinFunc("Function.prototype.bind", 1))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (FunctionProtoLoc, prop_proto)
  )

  val locclone = Shell.params.opt_LocClone
  
  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("Function.constructor"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          // update "length" property to this object.
          val h_1 = lset_this.foldLeft(HeapBot)((_h,l) =>
            _h + Helper.PropStore(h, l, AbsString.alpha("length"), Value(NumTop)))
          ((h_1, ctx), (he, ctxe))
        })),
      ("Function.prototype"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
        })),
      ("Function.prototype.toString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2._2
          val es =
            if (lset_this.exists((l) => h(l)("@class")._2._1._5 != AbsString.alpha("Function")))
              Set[Exception](TypeError)
            else
              ExceptionBot
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          
          // web bug-detector
          if(Shell.params.command == ShellParameters.CMD_WEBAPP_BUG_DETECTOR && es == ExceptionBot){
            val strval = lset_this.foldLeft(StrBot)((s, l_f) => {
              val o_f = h(l_f)
              o_f("@function")._3.foldLeft(s)((_s, fid) => {
                if(cfg.isUserFunction(fid)){ 
                  val ss = "function " + cfg.getFuncName(fid) + "() { }"
                  _s + AbsString.alpha(ss)
                }
                else {
                  val fullfunname = cfg.getFuncName(fid)
                  val funnames = fullfunname.split('.')
                  val funname = if(funnames.size == 0) fullfunname
                                else funnames(funnames.size -1)
                  val ss = "function " + funname + "() {\n    [native code]\n}"
                  _s + AbsString.alpha(ss)
                }
              
              })
            })
            ((Helper.ReturnStore(h, Value(strval)), ctx), (he + h_e, ctxe + ctx_e))
          }
          else
            ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he + h_e, ctxe + ctx_e))
        })),
      ("Function.prototype.apply"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2._2
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val addr2 = cfg.getAPIAddress(addr_env, 1)
          val addr3 = cfg.getAPIAddress(addr_env, 2)
          val addr4 = cfg.getAPIAddress(addr_env, 3)
//          val l_r1 = addrToLoc(addr1, Recent)
          val l_r2 = addrToLoc(addr2, Recent) // new pure local
          val l_r3 = addrToLoc(addr3, Recent) // newly generated arguments variable
          val (h_1, ctx_1) = (h, ctx) // Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2)
          val (h_3, ctx_3) = Helper.Oldify(h_2, ctx_2, addr3)
          val lset_this = h_3(SinglePureLocalLoc)("@this")._2._2

          // 1.
//          val cond = lset_this.exists((l) => BoolFalse <= Helper.IsCallable(h_3,l))
          val cond = lset_this.exists(l => {
            if(BoolFalse <= Helper.IsCallable(h_3,l)) BoolFalse <= Helper.IsBound(h_3,l)
            else false
          })
          val es1 =
            if (cond) Set[Exception](TypeError)
            else Set[Exception]()
          val lset_f = lset_this.filter((l) => BoolTrue <= Helper.IsCallable(h_3,l))
          val lset_tarf = lset_this.filter(l => BoolTrue <= Helper.IsBound(h_3,l))

          // 2. create empty Arguments object
          val v_arg = getArgValue(h_3, ctx_3, args, "1")
          val o_arg1 =
            if (v_arg._1._1 </ UndefBot || v_arg._1._2 </ NullBot)  Helper.NewArgObject(AbsNumber.alpha(0))
            else  Obj.bottom

          // 3.
          val v_arg1 = Value(PValue(UndefBot, NullBot, v_arg._1._3, v_arg._1._4, v_arg._1._5), v_arg._2)
          val (v_arg2, es2) =
            if (v_arg1._1 </ PValueBot)
              (Value(PValueBot, v_arg1._2), Set[Exception](TypeError))
            else
              (v_arg1, Set[Exception]())

          // 4. ~ 8. create Arguments object with argArray
          val o_arg2 =
            v_arg2._2.foldLeft(Obj.bottom)((_o, l) => {
              val n_arglen = Operator.ToUInt32(Helper.Proto(h_3, l, AbsString.alpha("length")))
              _o + (n_arglen.getAbsCase match {
                case AbsBot => Obj.bottom
                case _ => AbsNumber.getUIntSingle(n_arglen) match {
                  case Some(n_len) =>
                    val o = Helper.NewArgObject(AbsNumber.alpha(n_len))
                    (0 until n_len.toInt).foldLeft(o)((_o, i) => {
                      val value = Helper.Proto(h_3, l, AbsString.alpha(i.toString))
                      val propv = PropValue(ObjectValue(value, BoolTrue, BoolTrue, BoolTrue))
                      _o.update(i.toString, propv)
                    })
                  case _ =>
                    val value = Helper.Proto(h_3, l, NumStr)
                    val propv = PropValue(ObjectValue(value, BoolTrue, BoolTrue, BoolTrue))
                    Helper.NewArgObject(n_arglen).update(NumStr, propv)
                }
              })
            })
          val o_arg3_ = o_arg1 + o_arg2

          //////////////////////////////////////////////////
          // check whether there is a bound function or not
          val o_arg3 =
            if(lset_tarf.isEmpty) {
              o_arg3_
            } else {
              val lset_target_args = lset_tarf.foldLeft(LBot)((lset, l_tf) => {
                h_3(l_tf)("@bound_args")._2._2 ++ lset
              })
              val target_args = lset_target_args.foldLeft(Obj.bottom)((obj, l_ta) => obj + h_3(l_ta))
              Helper.concat(target_args, o_arg3_)
            }
          //////////////////////////////////////////////////

          val v_arg3 = Value(l_r3)
          val h_4 = h_3.update(l_r3, o_arg3)

          // *  in our own semantics, this value should be object
          val v_this = getArgValue(h_4, ctx_3, args, "0")
          val lset_argthis = Helper.getThis(h_4, v_this)
          val v_this2 =  Value(PValue(UndefBot, NullBot, v_this._1._3, v_this._1._4, v_this._1._5), lset_argthis)
          val (callee_this, h_5, ctx_5, es3) = Helper.toObject(h_4, ctx_3, v_this2, addr4)

          // XXX: stop if thisArg or arguments is LocSetBot(ValueBot)
          if(v_this2 == ValueBot || v_arg == ValueBot) ((h, ctx), (he, ctxe))
          else {
            val o_old = h_5(SinglePureLocalLoc)
            val cc_caller = cp._2
            val n_aftercall = cfg.getAftercallFromCall(cp._1)
            val cp_aftercall = (n_aftercall, cc_caller)
            val n_aftercatch = cfg.getAftercatchFromCall(cp._1)
            val cp_aftercatch = (n_aftercatch, cc_caller)
            
            //////////////////////////////////////////////////
            // check whether there is a bound function or not
            lset_tarf.foreach((l_f) => {
              val o_f = h_5(l_f)
              val l_this = o_f("@bound_this")._2._2
              val (fids, scope_locs) = o_f("@target_function")._2._2.foldLeft((FunSetBot, LocSetBot))((fidslocs, l) => ((fidslocs._1 ++ h_5(l)("@function")._3, fidslocs._2 ++ h_5(l)("@scope")._2._2)))
//              o_f("@target_function")._1._3.foreach((fid) => {
              fids.foreach((fid) => {
                cc_caller.NewCallContext(h, cfg, fid, l_r2, l_this).foreach((pair) => {
                  val (cc_new, o_new) = pair
                  val o_new2 = o_new.
                    update(cfg.getArgumentsName(fid),
                    PropValue(ObjectValue(v_arg3, BoolTrue, BoolFalse, BoolFalse))).
                    update("@scope", PropValue(ObjectValue(Value(scope_locs), BoolBot, BoolBot, BoolBot), FunSetBot))
//                    update("@scope", o_f("@scope"))
                  sem.addCallEdge(cp, ((fid,LEntry), cc_new), ContextEmpty, o_new2)
                  sem.addReturnEdge(((fid,LExit), cc_new), cp_aftercall, ctx_5, o_old)
                  sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_5, o_old)
                })
              })
            })
            //////////////////////////////////////////////////
            
            lset_f.foreach((l_f) => {
              val o_f = h_5(l_f)
              o_f("@function")._3.foreach((fid) => {
                cc_caller.NewCallContext(h, cfg, fid, l_r2, callee_this._2).foreach((pair) => {
                  val (cc_new, o_new) = pair
                  val o_new2 = o_new.
                    update(cfg.getArgumentsName(fid),
                    PropValue(ObjectValue(v_arg3, BoolTrue, BoolFalse, BoolFalse))).
                    update("@scope", o_f("@scope"))
                  sem.addCallEdge(cp, ((fid,LEntry), cc_new), ContextEmpty, o_new2)
                  sem.addReturnEdge(((fid,LExit), cc_new), cp_aftercall, ctx_5, o_old)
                  sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_5, o_old)
                })
              })
            })

            val (h_e, ctx_e) = Helper.RaiseException(h_5, ctx_5, es1++es2++es3)
            val s_1 = (he + h_e, ctxe + ctx_e)

            val h_6 = v_arg3._2.foldLeft(HeapBot)((hh, l) => {
              hh + h_5.update(l, h_5(l).update("callee",
                PropValue(ObjectValue(Value(lset_f ++ lset_tarf), BoolTrue, BoolFalse, BoolTrue))))
            })

            ((h_6, ctx_5), s_1)
          }})),
      ("Function.prototype.call"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2._2
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val addr2 = cfg.getAPIAddress(addr_env, 1)
          val addr2_1 = if (locclone && Config.loopSensitive) Helper.extendAddr(addr2, Helper.callContextToNumber(cp._2)) else addr2
          val addr3 = cfg.getAPIAddress(addr_env, 2)
          val addr4 = cfg.getAPIAddress(addr_env, 3)
          val l_r1 = addrToLoc(addr1, Recent) // newly generated arguments objects
          val l_r2 = addrToLoc(addr2_1, Recent) // new purelocal
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2_1)
          val lset_this = h_2(SinglePureLocalLoc)("@this")._2._2

          // 1.
//          val cond = lset_this.exists((l) => BoolFalse <= Helper.IsCallable(h_2,l))
          val cond = lset_this.exists(l => {
            if(BoolFalse <= Helper.IsCallable(h_1,l)) BoolFalse <= Helper.IsBound(h_1,l)
            else false
          })
          val es =
            if (cond) Set[Exception](TypeError)
            else Set[Exception]()
          val (h_e, ctx_e) = Helper.RaiseException(h_2, ctx_2, es)
          val lset_f = lset_this.filter((l) => BoolTrue <= Helper.IsCallable(h_2,l))
          val lset_tarf = lset_this.filter(l => BoolTrue <= Helper.IsBound(h_2,l))

          // 2., 3. create Arguments object
          val len = Operator.bopMinus(getArgValue(h_2, ctx_2, args, "length"), Value(AbsNumber.alpha(1)))
          val np = len._1._4
          val o_arg = Helper.NewArgObject(np)
          val o_arg_1_ =
            np.getSingle match {
              case Some(n) => (0 until n.toInt).foldLeft(o_arg)((_o, i) =>
                _o.update(AbsString.alpha(i.toString), PropValue(ObjectValue(getArgValue(h_2, ctx_2, args, (i+1).toString), BoolTrue, BoolTrue, BoolTrue))))
              case None =>
                if (np <= NumBot)
                  Obj.bottom
                else
                  o_arg.update(NumStr, PropValue(ObjectValue(getArgValueAbs(h_2, ctx_2, args, NumStr), BoolTrue, BoolTrue, BoolTrue)))
            }
          //////////////////////////////////////////////////
          // check whether there is a bound function or not
          val o_arg_1 =
            if(lset_tarf.isEmpty) {
              o_arg_1_
            } else {
              val lset_target_args = lset_tarf.foldLeft(LBot)((lset, l_tf) => {
                h_2(l_tf)("@bound_args")._2._2 ++ lset
              })
              val target_args = lset_target_args.foldLeft(Obj.bottom)((obj, l_ta) => obj + h_2(l_ta))
              Helper.concat(target_args, o_arg_1_)
            }
          //////////////////////////////////////////////////
          val h_3 = h_2.update(l_r1, o_arg_1)
          val v_arg = Value(l_r1)


          val v_this = getArgValue(h_3, ctx_2, args, "0")
          val lset_argthis = Helper.getThis(h_3, v_this)
          val v_this2 =  Value(PValue(UndefBot, NullBot, v_this._1._3, v_this._1._4, v_this._1._5), lset_argthis)
          val (callee_this, h_4, ctx_4, es3) = Helper.toObject(h_3, ctx_2, v_this2, addr4)


          val o_old = h_4(SinglePureLocalLoc)
          val cc_caller = cp._2
          val n_aftercall = cfg.getAftercallFromCall(cp._1)
          val cp_aftercall = (n_aftercall, cc_caller)
          val n_aftercatch = cfg.getAftercatchFromCall(cp._1)
          val cp_aftercatch = (n_aftercatch, cc_caller)
          //////////////////////////////////////////////////
          // check whether there is a bound function or not
          lset_tarf.foreach((l_f) => {
            val o_f = h_4(l_f)
            val l_this = o_f("@bound_this")._2._2
            val (fids, scope_locs) = o_f("@target_function")._2._2.foldLeft((FunSetBot, LocSetBot))((fidslocs, l) => ((fidslocs._1 ++ h_4(l)("@function")._3, fidslocs._2 ++ h_4(l)("@scope")._2._2)))
//            o_f("@target_function")._1._3.foreach((fid) => {
            fids.foreach((fid) => {
              cc_caller.NewCallContext(h, cfg, fid, l_r2, l_this, Some(addr2)).foreach((pair) => {
                val (cc_new, o_new) = pair
                val o_new2 = o_new.
                  update(cfg.getArgumentsName(fid),
                  PropValue(ObjectValue(v_arg, BoolTrue, BoolFalse, BoolFalse))).
//                  update("@scope", o_f("@scope"))
                    update("@scope", PropValue(ObjectValue(Value(scope_locs), BoolBot, BoolBot, BoolBot), FunSetBot))
                sem.addCallEdge(cp, ((fid,LEntry), cc_new), ContextEmpty, o_new2)
                sem.addReturnEdge(((fid,LExit), cc_new), cp_aftercall, ctx_4, o_old)
                sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_4, o_old)
              })
            })
          })
          //////////////////////////////////////////////////
          lset_f.foreach((l_f) => {
            val o_f = h_4(l_f)
            o_f("@function")._3.foreach((fid) => {
              cc_caller.NewCallContext(h, cfg, fid, l_r2, callee_this._2, Some(addr2)).foreach((pair) => {
                val (cc_new, o_new) = pair
                val o_new2 = o_new.
                  update(cfg.getArgumentsName(fid),
                  PropValue(ObjectValue(v_arg, BoolTrue, BoolFalse, BoolFalse))).
                  update("@scope", o_f("@scope"))
                sem.addCallEdge(cp, ((fid,LEntry), cc_new), ContextEmpty, o_new2)
                sem.addReturnEdge(((fid,LExit), cc_new), cp_aftercall, ctx_4, o_old)
                sem.addReturnEdge(((fid, LExitExc), cc_new), cp_aftercatch, ctx_4, o_old)
              })
            })
          })
          val h_5 = v_arg._2.foldLeft(HeapBot)((hh, l) => {
            hh + h_4.update(l, h_4(l).update("callee",
              PropValue(ObjectValue(Value(lset_f ++ lset_tarf), BoolTrue, BoolFalse, BoolTrue))))
          })
          val s_1 = (he + h_e, ctxe + ctx_e)
          ((h_5, ctx_4), s_1)
        })),
      ("Function.prototype.bind"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2._2
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0) // new internal list of argument values
          val addr2 = cfg.getAPIAddress(addr_env, 1) // return object(bound function object)
          val l_r1 = addrToLoc(addr1, Recent)
          val l_r2 = addrToLoc(addr2, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2)

          val lset_this = h_2(SinglePureLocalLoc)("@this")._2._2

          // 1. && 2.
          val cond = lset_this.exists((l) => !((BoolFalse <= (Helper.IsCallable(h_2,l))) || (BoolFalse <=  Helper.IsBound(h_2, l))))
          val es =
            if (cond) Set[Exception](TypeError)
            else Set[Exception]()
          val (h_e, ctx_e) = Helper.RaiseException(h_2, ctx_2, es)
          val lset_f = lset_this.filter((l) => BoolTrue <= Helper.IsCallable(h_2,l))
          val lset_tarf = lset_this.filter((l) => BoolTrue <= Helper.IsBound(h_2,l))
          // compute target function fids
//          val target_fids = (lset_f++lset_tarf).foldLeft(FunSetBot)((fidset, l_f) => h_2(l_f)("@function")._3 ++ h_2(l_f)("@target_function")._1._3 ++ fidset)
          // compute target locations which is function object
          val target_functions_locs = lset_f++lset_tarf.foldLeft(LocSetBot)((locs, l_f) => {
            h_2(l_f)("@target_function")._2._2 ++
            locs
          })
          
          // 3. new internal list of all of the argument values
          // remove this binding property('0') from length
          val origin_len = (lset_f ++ lset_tarf).foldLeft(ValueBot)((v_len, l_f) => h_2(l_f)("length")._1._1 + v_len)
          val len = 
              Operator.bopMinus(getArgValue(h_2, ctx_2, args, "length"), Value(AbsNumber.alpha(1)))
          val np = len._1._4
          val o_arg = Helper.NewArgObject(np)
          
          // compute previously bounded "@bound_args" by bind function
          val prev_bound_arg =
            if(!lset_tarf.isEmpty) {
              lset_tarf.foldLeft(Obj.bottom)((obj, l_f) => {
                h_2(l_f)("@bound_args")._2._2.foldLeft(obj)((obj_, l_arg) => obj_ + h_2(l_arg))
              })
            }
            else {
              // no bounded arguments
              Helper.NewArgObject(AbsNumber.alpha(0))
            }
          
          val o_arg_1 = 
            np.getSingle match {
              case Some(n) => (0 until n.toInt).foldLeft(o_arg)((_o, i) =>
                _o.update(AbsString.alpha(i.toString), PropValue(ObjectValue(getArgValue(h_2, ctx_2, args, (i+1).toString), BoolTrue, BoolTrue, BoolTrue))))
              case None =>
                if (np <= NumBot)
                  Obj.bottom
                else
                  o_arg.update(NumStr, PropValue(ObjectValue(getArgValueAbs(h_2, ctx_2, args, NumStr), BoolTrue, BoolTrue, BoolTrue)))
            }
          val new_arg = Helper.concat(prev_bound_arg, o_arg_1)
          val h_3 = h_2.update(l_r1, new_arg)
          val v_arg = Value(l_r1)

          val v_this = getArgValue(h_3, ctx_2, args, "0") // + prev_this

          // 15. length ~ 17.
          // origin_len - len
          val o_f_len = Operator.bopMinus(origin_len, len)

          // 4. F be a new native ECAMScript object
          // make bound function object 7~15
          val o_f = Obj.empty.
//                        update("@target_function", PropValue(ObjectValueBot, ValueBot, target_fids)). // 7. [[TargetFunction]]
                        update("@target_function", PropValue(ObjectValue(Value(PValueBot, target_functions_locs), BoolBot, BoolBot, BoolBot), FunSetBot)). // 7. [[TargetFunction]]
                        update("@bound_this", PropValue(v_this)). // 8. [[BoundThis]]
                        update("@bound_args", PropValue(v_arg)). // 9. [[BoundArgs]]
                        update("@class", PropValue(AbsString.alpha("Function"))). // 10. [[Class]]
                        update("@proto", PropValue(ObjectValue(Value(FunctionProtoLoc), BoolFalse, BoolFalse, BoolFalse))). // 11. [[Prototype]]
                        update("length", PropValue(ObjectValue(o_f_len, BoolTrue, BoolFalse, BoolFalse))). // 15~17. length
                        update("@extensible", PropValue(BoolTrue)) // 18. [[Extensible]]
          // 12. [[Call]] affects semantics of CFGCall...?
          // 13. [[Construct]] ??
          // 14. [[HasInstance]]
          // 19. ~ 21 is for getter/setter of caller/arguments
          
          val h_4 = h_3.update(l_r2, o_f)
          val s_1 = (he + h_e, ctxe + ctx_e)
          ((Helper.ReturnStore(h_4, Value(l_r2)), ctx), s_1)
        }))
    )
  }

}
