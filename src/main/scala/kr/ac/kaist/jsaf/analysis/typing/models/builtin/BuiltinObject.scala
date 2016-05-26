/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing.models.builtin

import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, InternalError, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.bug_detector.BugKind
import kr.ac.kaist.jsaf.bug_detector.ToPropertyDescriptor
import kr.ac.kaist.jsaf.bug_detector.ToPropertyDescriptors
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object BuiltinObject extends ModelData {

  val ConstLoc = newSystemLoc("ObjectConst", Recent)
  //val ProtoLoc = newPreDefLoc("ObjectProto", Recent)

  private val prop_const: List[(String, AbsProperty)] = List(
    ("@class",                   AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto",                   AbsConstValue(PropValue(ObjectValue(Value(FunctionProtoLoc), F, F, F)))),
    ("@extensible",              AbsConstValue(PropValue(T))),
    ("@scope",                   AbsConstValue(PropValueNullTop)),
    ("@function",                AbsInternalFunc("Object")),
    ("@construct",               AbsInternalFunc("Object.constructor")),
    ("@hasinstance",             AbsConstValue(PropValueNullTop)),
    ("prototype",                AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("length",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, F, F)))),
    ("getPrototypeOf",           AbsBuiltinFunc("Object.getPrototypeOf", 1)),
    ("getOwnPropertyDescriptor", AbsBuiltinFunc("Object.getOwnPropertyDescriptor", 2)),
    ("getOwnPropertyNames",      AbsBuiltinFunc("Object.getOwnPropertyNames", 1)),
    ("create",                   AbsBuiltinFunc("Object.create", 2)),
    ("defineProperty",           AbsBuiltinFunc("Object.defineProperty", 3)),
    ("defineProperties",         AbsBuiltinFunc("Object.defineProperties", 2)),
    ("seal",                     AbsBuiltinFunc("Object.seal", 1)),
    ("freeze",                   AbsBuiltinFunc("Object.freeze", 1)),
    ("preventExtensions",        AbsBuiltinFunc("Object.preventExtensions", 1)),
    ("isSealed",                 AbsBuiltinFunc("Object.isSealed", 1)),
    ("isFrozen",                 AbsBuiltinFunc("Object.isFrozen", 1)),
    ("isExtensible",             AbsBuiltinFunc("Object.isExtensible", 1)),
    ("keys",                     AbsBuiltinFunc("Object.keys", 1))
  )

  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class",               AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto",               AbsConstValue(PropValue(ObjectValue(PValue(NullTop), F, F, F)))),
    ("@extensible",          AbsConstValue(PropValue(BoolTrue))),
    ("constructor",          AbsConstValue(PropValue(ObjectValue(ConstLoc, T, F, T)))),
    ("toString",             AbsBuiltinFunc("Object.prototype.toString", 0)),
    ("toLocaleString",       AbsBuiltinFunc("Object.prototype.toLocaleString", 0)),
    ("valueOf",              AbsBuiltinFunc("Object.prototype.valueOf", 0)),
    ("hasOwnProperty",       AbsBuiltinFunc("Object.prototype.hasOwnProperty", 1)),
    ("isPrototypeOf",        AbsBuiltinFunc("Object.prototype.isPrototypeOf", 1)),
    ("propertyIsEnumerable", AbsBuiltinFunc("Object.prototype.propertyIsEnumerable", 1))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (ConstLoc, prop_const), (ObjProtoLoc, prop_proto)
  )

  def toPropertyDescriptor(h: Heap, v_2: Value, toDescriptor: BugKind, abstraction: Boolean) =
    // 8.10.5 1.
    if ((v_2.pv </ PValueBot || v_2.locs.isEmpty) && Config.typingInterface != null) {
      if(Shell.params.opt_DeveloperMode || !abstraction)
        Config.typingInterface.signal(Config.typingInterface.getSpan, toDescriptor, v_2.pv.toString, null)
      Set[Exception](TypeError)
    // 8.10.5 7.b.
    } else {
      v_2.locs.toSet.find(l => if (BoolTrue <= Helper.HasProperty(h, l, AbsString.alpha("get"))) {
                               val getter = Helper.Proto(h, l, AbsString.alpha("get"))
                               getter.locs.toSet.find(ll =>
                                         BoolFalse <= Helper.IsCallable(h, ll) &&
                                         getter.pv._1 <= UndefBot).isDefined ||
                                         getter.pv._1 <= UndefBot && getter.pv._1 <= UndefBot
                             } else false) match {
        case Some(l) =>
          val getter = Helper.Proto(h, l, AbsString.alpha("get"))
          val get = getter.locs.toSet.find(ll => BoolFalse <= Helper.IsCallable(h, ll) &&
                                               getter.pv._1 <= UndefBot) match {
                      case Some(ll) => h(ll).toString
                      case None => getter.pv.toString
                    }

          if(Shell.params.opt_DeveloperMode || !abstraction)
            Config.typingInterface.signal(Config.typingInterface.getSpan, toDescriptor, get, null)
          Set[Exception](TypeError)
        case _ =>
          // 8.10.5 8.b.
          v_2.locs.toSet.find(l => if (BoolTrue <= Helper.HasProperty(h, l, AbsString.alpha("set"))) {
                                   val setter = Helper.Proto(h, l, AbsString.alpha("set"))
                                   setter.locs.toSet.find(ll =>
                                         BoolFalse <= Helper.IsCallable(h, ll) &&
                                         setter.pv._1 <= UndefBot).isDefined ||
                                         setter.locs.isEmpty && setter.pv._1 <= UndefBot
                                 } else false) match {
            case Some(l) =>
              val setter = Helper.Proto(h, l, AbsString.alpha("set"))
              val set = setter.locs.toSet.find(ll => BoolFalse <= Helper.IsCallable(h, ll) &&
                                                   setter.pv._1 <= UndefBot) match {
                          case Some(ll) => h(ll).toString
                          case None => setter.pv.toString
                        }
              if(Shell.params.opt_DeveloperMode || !abstraction)
                Config.typingInterface.signal(Config.typingInterface.getSpan, toDescriptor, set, null)
              Set[Exception](TypeError)
            case _ =>
              // 8.10.5 9.a.
              v_2.locs.toSet.find(l => BoolTrue <= Helper.HasProperty(h, l, AbsString.alpha("value"))) match {
                case Some(l) =>
                  if(Shell.params.opt_DeveloperMode || !abstraction)
                    Config.typingInterface.signal(Config.typingInterface.getSpan, toDescriptor,
                                                Helper.Proto(h, l, AbsString.alpha("value")).pv.toString, null)
                  Set[Exception](TypeError)
                case _ =>
                  v_2.locs.toSet.find(l => BoolTrue <= Helper.HasProperty(h, l, AbsString.alpha("writable"))) match {
                    case Some(l) =>
                      if(Shell.params.opt_DeveloperMode || !abstraction)
                        Config.typingInterface.signal(Config.typingInterface.getSpan, toDescriptor,
                                                    Helper.Proto(h, l, AbsString.alpha("writable")).pv.toString, null)
                      Set[Exception](TypeError)
                    case _ =>
                      ExceptionBot
                  }
              }
            }
          }
    }

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      "Object" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          // 15.2.2.1 new Object( [value] )
          val v = getArgValue(h, ctx, args, "0") // [value]

          // 1.a. If Type(value) is Object, then simply return value.
          //      We do not consider an implementation-dependent actions for a host object.
          val (v_1, h_1, ctx_1) =
            if (!v.locs.isEmpty) (Value(v.locs), h, ctx)
            else (ValueBot, HeapBot, ContextBot)

          // 1.b. If Type(value) is String, return ToObject(value)
          // 1.c. If Type(value) is Boolean, return ToObject(value)
          // 1.d. If Type(value) is Number, return ToObject(value)
          val (v_2, h_2, ctx_2, es) =
            if ((v.pv._3 </ BoolBot) || (v.pv._4 </ NumBot) || (v.pv._5 </ StrBot)) {
              val _v_new = Value(PValue(UndefBot, NullBot, v.pv._3, v.pv._4, v.pv._5))
              val (_v, _h, _ctx, _es) = Helper.toObject(h, ctx, _v_new, addr1)
              (_v, _h, _ctx, _es)
            } else {
              (ValueBot, HeapBot, ContextBot, ExceptionBot)
            }
          // 2. Assert: The argument value was not supplied or its type was Null or Undefined.
          val (v_3, h_3, ctx_3) =
            if ((v.pv._1 </ UndefBot) || (v.pv._2 </ NullBot)) {
              val (_h_1, _ctx_1) = Helper.Oldify(h, ctx, addr1)
              val _l_r = addrToLoc(addr1, Recent)
              val _h = Helper.allocObject(_h_1, ObjProtoSingleton, _l_r)
              (Value(_l_r), _h, _ctx_1)
            } else {
              (ValueBot, HeapBot, ContextBot)
            }

          val v_4 = v_1 + v_2 + v_3
          val h_4 = h_1 + h_2 + h_3
          val ctx_4 = ctx_1 + ctx_2 + ctx_3

          val (h_e, ctx_e) = Helper.RaiseException(h_2, ctx_2, es)
          val s = (he + h_e, ctxe + ctx_e)

          if (v_4 </ ValueBot)
            ((Helper.ReturnStore(h_4, v_4), ctx_4), s)
          else
            ((HeapBot, ContextBot), s)
        }),
      ("Object.constructor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          // 15.2.2.1 new Object( [value] )
          val v = getArgValue(h, ctx, args, "0") // [value]

          // 1.a. If Type(value) is Object, then simply return value.
          //      We do not consider an implementation-dependent actions for a host object.
          val (v_1, h_1, ctx_1) =
            if (!v.locs.isEmpty) (Value(v.locs), h, ctx)
            else (ValueBot, HeapBot, ContextBot)

          // 1.b. If Type(value) is String, return ToObject(value)
          // 1.c. If Type(value) is Boolean, return ToObject(value)
          // 1.d. If Type(value) is Number, return ToObject(value)
          val (v_2, h_2, ctx_2) =
            if ((v.pv._3 </ BoolBot) || (v.pv._4 </ NumBot) || (v.pv._5 </ StrBot)) {
              val _v_new = Value(PValue(UndefBot, NullBot, v.pv._3, v.pv._4, v.pv._5))
              val o_1 =
                if (!(_v_new.pv._5 <= StrBot)) Helper.NewString(v.pv._5)
                else Obj.bottom
              val o_2 =
                if (!(_v_new.pv._3 <= BoolBot)) Helper.NewBoolean(v.pv._3)
                else Obj.bottom
              val o_3 =
                if (!(_v_new.pv._4 <= NumBot)) Helper.NewNumber(v.pv._4)
                else Obj.bottom
              val o = o_1 + o_2 + o_3
              val _h = lset_this.foldLeft(HeapBot)((_h, l) => _h + h.update(l, o))
              (Value(lset_this), _h, ctx)
            } else {
              (ValueBot, HeapBot, ContextBot)
            }
          // 2. Assert: The argument value was not supplied or its type was Null or Undefined.
          val (v_3, h_3, ctx_3) =
            if ((v.pv._1 </ UndefBot) || (v.pv._2 </ NullBot)) {
              val _h = lset_this.foldLeft(HeapBot)((_h, l) => _h + Helper.allocObject(h, ObjProtoSingleton, l))
              (Value(lset_this), _h, ctx)
            } else {
              (ValueBot, HeapBot, ContextBot)
            }

          val v_4 = v_1 + v_2 + v_3
          val h_4 = h_1 + h_2 + h_3
          val ctx_4 = ctx_1 + ctx_2 + ctx_3
          if (v_4 </ ValueBot)
            ((Helper.ReturnStore(h_4, v_4), ctx_4), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.getPrototypeOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val es =
            if (v_1.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val v_2 = v_1.locs.foldLeft(ValueBot)(
            (_v, l) => _v + h(l)("@proto")._1._1)
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_1, ctx_1) =
            if (v_2 </ ValueBot) (Helper.ReturnStore(h,v_2), ctx)
            else (HeapBot, ContextBot)
          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        })),
      "Object.getOwnPropertyDescriptor" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val s_prop = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          val es =
            if (v_1.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val (propv, absent) = v_1.locs.foldLeft[(PropValue, AbsBool)]((PropValueBot, BoolBot))((pva, l) => {
            val pv = h(l)(s_prop)
            val a = h(l).domIn(s_prop)
            (pva._1 + pv, pva._2 + a)})
          val (v_2, h_2, ctx_2) =
            if (BoolFalse <= absent || propv <= PropValueBot )
              (Value(UndefTop), h, ctx)
            else
              (ValueBot, HeapBot, ContextBot)
          val ov = propv._1
          val (v_3, h_3, ctx_3) =
            if (Value(PValue(UndefBot, ov._1.pv._2, ov._1.pv._3, ov._1.pv._4, ov._1.pv._5), ov._1.locs) </ ValueBot) {
              val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
              val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
              if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
              val addr_env = (cp._1._1, set_addr.head)
              val addr1 = cfg.getAPIAddress(addr_env, 0)
              val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
              val l_r = addrToLoc(addr1, Recent)
              val o_new = Helper.NewObject(ObjProtoLoc)
              val o_1 =
                if (true) // isDataDescriptor(H(v),s)
                  o_new.
                    update("value", PropValue(ObjectValue(ov._1, BoolTrue, BoolTrue, BoolTrue))).
                    update("writable", PropValue(ObjectValue(ov._2, BoolTrue, BoolTrue, BoolTrue)))
                else
                  o_new
              val o_2 = o_1.
                update("enumerable", PropValue(ObjectValue(ov._3, BoolTrue, BoolTrue, BoolTrue))).
                update("configurable", PropValue(ObjectValue(ov._4, BoolTrue, BoolTrue, BoolTrue)))
              val h_2 = h_1.update(l_r, o_2)
              (Value(LocSet(l_r)), h_2, ctx_1)
            }
            else
              (ValueBot, HeapBot, ContextBot)
          val v_4 = v_2 + v_3
          val h_4 = h_2 + h_3
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_5, ctx_5) =
            if (v_4 </ ValueBot) (Helper.ReturnStore(h_4, v_4), ctx_2 + ctx_3)
            else (HeapBot, ContextBot)
          ((h_5, ctx_5), (he + h_e, ctxe + ctx_e))
        }),
      ("Object.getOwnPropertyNames" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val v = getArgValue(h_1, ctx_1, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val o = v.locs.foldLeft(Obj.bottom)((_o, l) => {
            val o_new = Helper.NewArrayObject(AbsNumber.alpha(h_1(l).getProps.size))
            val o_1 = h_1(l).getProps.foldLeft(o_new)((_o, s) => _o.update(NumStr, PropValue(ObjectValue(AbsString.alpha(s),BoolTrue,BoolTrue,BoolTrue))))
            val o_2 =
              if (h_1(l)(Str_default_number) </ PropValueBot)
                o_new.update(NumStr, PropValue(ObjectValue(NumStr,BoolTrue,BoolTrue,BoolTrue)))
              else
                Obj.bottom
            val o_3 =
              if (h_1(l)(Str_default_other) </ PropValueBot)
                o_new.update(NumStr, PropValue(ObjectValue(OtherStr,BoolTrue,BoolTrue,BoolTrue)))
              else
                Obj.bottom
            o_1 + o_2 + o_3
          })
          val (h_e, ctx_e) = Helper.RaiseException(h_1, ctx_1, es)
          if (o </ Obj.bottom) {
            val h_2 = h_1.update(l_r, o)
            ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he+h_e, ctxe+ctx_e))
          }
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      "Object.create" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h1, ctx1) = Helper.Oldify(h, ctx, addr1)

          val v_1 = getArgValue(h1, ctx1, args, "0")
          val v_2 = getArgValue(h1, ctx1, args, "1")
          // 1. If Type(O) is not Object or Null throw a TypeError exception.
          val es_1 = if (v_1.pv._1 </ UndefBot || v_1.pv._3 </ BoolBot || v_1.pv._4 </ NumBot || v_1.pv._5 </ StrBot) Set[Exception](TypeError)
                     else ExceptionBot
          // 2. Let obj be the result of creating a new object as if by the expression new Object() where Object
          //    is the standard built-in constructor with that name
          // 3. Set the [[Prototype]] internal property of obj to O.
          val o_1 =
            if (v_1.pv._2 </ NullBot) Helper.NewObject()
            else Obj.bottom
          val o_2 =
            if (!v_1.locs.isEmpty) Helper.NewObject(v_1.locs)
            else Obj.bottom
          val o = o_1 + o_2

          val (lset, h_1, ctx_1) =
            if (o </ Obj.bottom) {
              val h_2_ = h1.update(l_r, o)
              // 4. If the argument Properties is present and not undefined, add own properties to obj as if by calling
              //    the standard built-in function Object.defineProperties with arguments obj and Properties.
              val h_3_ =
                if (!v_2.locs.isEmpty) {
                  val newobj = v_2.locs.foldLeft(h_2_(l_r))((_obj, l_2) => _obj + Helper.DefineProperties(h_2_, l_r, l_2))
                  h_2_.update(l_r, newobj)
                }
                else
                  h_2_

              (LocSet(l_r), h_3_, ctx1)
            } else {
              (LocSetBot, HeapBot, ContextBot)
            }

          // 4. If the argument Properties is present and not undefined, add own properties to obj as if by calling
          //    the standard built-in function Object.defineProperties with arguments obj and Properties.
          val es_2 =
            if (v_2.pv._2 </ NullBot || v_2.pv._3 </ BoolBot || v_2.pv._4 </ NumBot || v_2.pv._5 </ StrBot) Set[Exception](TypeError)
            else ExceptionBot

          val es = es_1 ++ es_2
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((Helper.ReturnStore(h_1, Value(lset)), ctx_1), (he + h_e, ctxe + ctx_e))
        }),
      // 15.2.3.6
      ("Object.defineProperty" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val lset_callee = getArgValue(h, ctx, args, "callee").locs
          val abstraction = (lset_callee.size > 1)
          // 1.
          val es_1 =
            if (v_1.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          // 2.
          val s_name = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          // 3.
          val v_2 = getArgValue(h, ctx, args, "2")
          val es_2 = toPropertyDescriptor(h, v_2, ToPropertyDescriptor, abstraction)
          
          // 4.
          val h_1 =
            v_1.locs.foldLeft(h)((_h, l_1) => {
              val newobj = v_2.locs.foldLeft(h(l_1))((__obj, l_2) =>
                __obj + Helper.DefineProperty(h, l_1, s_name, l_2)) 
              _h.update(l_1, newobj)
            })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es_1 ++ es_2)
          if (Value(v_1.locs) </ ValueBot)
            ((Helper.ReturnStore(h_1, Value(v_1.locs)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      // 15.2.3.7
      ("Object.defineProperties" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v_1 = getArgValue(h, ctx, args, "0")
          val lset_callee = getArgValue(h, ctx, args, "callee").locs
          val abstraction = (lset_callee.size > 1)
          // 1.
          val es_1 =
            if (v_1.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          // 2.
          val v_2 = getArgValue(h, ctx, args, "1")
          val es_2 =
            if (v_2.pv </ PValueBot) {
              if (Config.typingInterface != null)
                if(Shell.params.opt_DeveloperMode || !abstraction)
                  Config.typingInterface.signal(Config.typingInterface.getSpan, ToPropertyDescriptor, v_2.pv.toString, null)
              Set[Exception](TypeError)
            } else {
              v_2.locs.foldLeft(ExceptionBot)((x, l) =>
                                          {try{Helper.CollectOwnProps(h, LocSet(l))} catch{
                                             case e: InternalError => {h(l).getProps}}
                                          }.foldLeft(x)((x, s) => {
                                               val prop = AbsString.alpha(s)
                                               val v__1 = Helper.Proto(h, l, prop)
                                               v__1.locs.foldLeft(x)((x, ll) => x++toPropertyDescriptor(h, v__1, ToPropertyDescriptors, abstraction))}))
            }
          val h_1 =
            v_1.locs.foldLeft(h)((_h, l_1) => {
              val newobj = v_2.locs.foldLeft(h(l_1))((__obj, l_2) => __obj + Helper.DefineProperties(h, l_1, l_2))
              _h.update(l_1, newobj)
            })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es_1 ++ es_2)
          if (Value(v_1.locs) </ ValueBot)
            ((Helper.ReturnStore(h_1, Value(v_1.locs)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.seal" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val h_1 = v.locs.foldLeft(HeapBot)((_h, l) => {
            val obj = h(l)
            val obj_1 = obj.getProps.foldLeft(obj)((_o, s) => {
              val ov = _o(s)._1
              _o.update(s, PropValue(ObjectValue(ov._1,ov._2,ov._3,BoolFalse)))
            })
            val obj_2 = obj_1.update("@extensible", PropValue(BoolFalse))
            _h + h.update(l, obj_2)
          })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (Value(v.locs) </ ValueBot)
            ((Helper.ReturnStore(h_1, Value(v.locs)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.freeze" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val h_1 = v.locs.foldLeft(HeapBot)((_h, l) => {
            val obj = h(l)
            val obj_1 = obj.getProps.foldLeft(obj)((_o, s) => {
              val ov = _o(s)._1
              _o.update(s, PropValue(ObjectValue(ov._1,BoolFalse,ov._3,BoolFalse)))
            })
            val obj_2 = obj_1.update("@extensible", PropValue(BoolFalse))
            _h + h.update(l, obj_2)
          })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (Value(v.locs) </ ValueBot)
            ((Helper.ReturnStore(h_1, Value(v.locs)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.preventExtensions" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val h_1 = v.locs.foldLeft(HeapBot)((_h, l) =>
            _h + h.update(l, h(l).update("@extensible", PropValue(BoolFalse))))
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (Value(v.locs) </ ValueBot)
            ((Helper.ReturnStore(h_1, Value(v.locs)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.isSealed" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val b = v.locs.foldLeft[AbsBool](BoolBot)((_b, l) => {
            val o = h(l)
            val props = o.getProps
            val b_f =
              if (props.exists((s) => BoolTrue <= o(s)._1._4))
                BoolFalse
              else  BoolBot
            val b_t =
              if (props.forall((s) => BoolFalse <= o(s)._1._4)) {
                val v_ex = o("@extensible")._2
                if (Value(BoolTop) <= v_ex)  BoolTop
                else if (Value(BoolFalse) <= v_ex) BoolTrue
                else if (Value(BoolTrue) <= v_ex) BoolFalse
                else BoolBot
              }
              else
                BoolBot
            _b + b_f + b_t
          })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.isFrozen" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val b = v.locs.foldLeft[AbsBool](BoolBot)((_b, l) => {
            val o = h(l)
            val props = o.getProps
            val b_f =
              if (props.exists((s) => (BoolTrue <= o(s)._1._2 || BoolTrue <= o(s)._1._4)))
                BoolFalse
              else
                BoolBot
            val b_t =
              if (props.forall((s) => (BoolFalse <= o(s)._1._2 && BoolFalse <= o(s)._1._4))) {
                val v_ex = o("@extensible")._2
                if (Value(BoolTop) <= v_ex)  BoolTop
                else if (Value(BoolFalse) <= v_ex) BoolTrue
                else if (Value(BoolTrue) <= v_ex) BoolFalse
                else BoolBot
              }
              else
                BoolBot
            _b + b_f + b_t
          })
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      ("Object.isExtensible" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val v = getArgValue(h, ctx, args, "0")
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot
          val v_ex = v.locs.foldLeft[Value](ValueBot)((_v, l) =>
            _v + h(l)("@extensible")._2)
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          if (v </ ValueBot)
            ((Helper.ReturnStore(h, v_ex), ctx), (he+h_e, ctxe+ctx_e))
          else
            ((HeapBot, ContextBot), (he+h_e, ctxe+ctx_e))
        })),
      "Object.keys" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)

          val v = getArgValue(h, ctx, args, "0")
          // 1. If the Type(O) is not Object, throw a TypeError exception.
          val es =
            if (v.pv </ PValueBot) Set[Exception](TypeError)
            else ExceptionBot

          val (h_2, ctx_2) =
            if (!v.locs.isEmpty) {
              val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
              val o = try {
                val list = Helper.CollectOwnProps(h, v.locs).toArray
                val o_new = Helper.NewArrayObject(AbsNumber.alpha(list.size))

                val o_1 = (0 to list.length - 1).foldLeft(o_new)((_o, i) => {
                  _o.update(AbsString.alpha(i.toString), PropValue(ObjectValue(AbsString.alpha(list(i)), BoolTrue, BoolTrue, BoolTrue)))
                })
                o_1
              } catch {
                case e: InternalError => {
                  v.locs.foldLeft(Obj.bottom)((_o, l) => {
                    val map_enum = h_1(l).getProps.filter((kv) => BoolTrue <= h_1(l)(kv)._1._3 && !(kv.take(1) == "@"))
                    val o_new = Helper.NewArrayObject(UInt)
                    val o_1 = map_enum.foldLeft(o_new)((_o, kv) => _o.update(NumStr, PropValue(ObjectValue(AbsString.alpha(kv), BoolTrue, BoolTrue, BoolTrue))))
                    val o_2 =
                      if (h_1(l)(Str_default_number) </ PropValueBot)
                        o_new.update(NumStr, PropValue(ObjectValue(NumStr, BoolTrue, BoolTrue, BoolTrue)))
                      else
                        Obj.bottom
                    val o_3 =
                      if (h_1(l)(Str_default_other) </ PropValueBot)
                        o_new.update(NumStr, PropValue(ObjectValue(OtherStr, BoolTrue, BoolTrue, BoolTrue)))
                      else
                        Obj.bottom
                    o_1 + o_2 + o_3
                  })
                }
              }
              val h_2 = h_1.update(l_r, o)
              ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1))
            } else {
              (HeapBot, ContextBot)
            }

          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          ((h_2, ctx_2), (he + h_e, ctxe + ctx_e))
        }),
      ("Object.prototype.toString"-> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val s = lset_this.foldLeft[AbsString](StrBot)((_s, l) => {
            val absstr = h(l)("@class")._2.pv._5
            _s + (absstr.getAbsCase match {
              case AbsSingle =>
                AbsString.alpha("[object " + absstr.getSingle.get + "]")
              case AbsBot =>
                StrBot
              case _ =>
                OtherStr
                })})
          if (s </ StrBot)
            ((Helper.ReturnStore(h, Value(s)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.prototype.toLocaleString" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val s = lset_this.foldLeft[AbsString](StrBot)((_s, l) => {
            val absstr = h(l)("@class")._2.pv._5
            _s + (absstr.getAbsCase match {
              case AbsSingle =>
                AbsString.alpha("[object " + absstr.getSingle.get + "]")
              case AbsBot =>
                StrBot
              case _ =>
                OtherStr
                })})
          if (s </ StrBot)
            ((Helper.ReturnStore(h, Value(s)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.prototype.valueOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          if (Value(lset_this) </ ValueBot)
            ((Helper.ReturnStore(h, Value(lset_this)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.prototype.hasOwnProperty" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          // 15.2.4.5 Object.prototype.hasOwnProperty(V)
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val b = lset_this.foldLeft[AbsBool](BoolBot)((b,l) => {
            b + Helper.HasOwnProperty(h, l, s)
          })
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.prototype.isPrototypeOf" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val v = getArgValue(h, ctx, args, "0")
          val b_1 =
            if (v.pv </ PValueBot)
              BoolFalse
            else
              BoolBot
          val b_2 = v.locs.foldLeft[AbsBool](BoolBot)((b, l) => {
            val v_proto = h(l)("@proto")._1._1
            val b_3 =
              if (NullTop <= v_proto.pv._2)
                BoolFalse
              else
                BoolBot
            val b_4 = Operator.bopEq(Value(lset_this), Value(v_proto.locs)).pv._3
            b + b_3 + b_4})
          val b = b_1 + b_2
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Object.prototype.propertyIsEnumerable" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val b =
            lset_this.foldLeft[AbsBool](BoolBot)((_b, l) => {
              val ov = h(l)(s)._1._1
              val hasProp = Helper.HasProperty(h, l, s)
              val b_1 =
                if (BoolFalse <= hasProp)
                  BoolFalse
                else
                  BoolBot
              val b_2 =
                if (BoolTrue <= hasProp)
                  Helper.ProtoProp(h, l, s)._1._3
                else
                  BoolBot
              _b + b_1 + b_2
            })
          if (b </ BoolBot)
            ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }))
    )
  }

}
