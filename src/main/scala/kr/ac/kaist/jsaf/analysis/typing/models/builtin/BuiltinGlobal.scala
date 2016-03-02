/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ******************************************************************************/
package kr.ac.kaist.jsaf.analysis.typing.models.builtin

import kr.ac.kaist.jsaf.{Shell, ShellParameters}
import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper=>AH}
import kr.ac.kaist.jsaf.analysis.typing.domain.Heap
import kr.ac.kaist.jsaf.analysis.typing.domain.Context
import kr.ac.kaist.jsaf.bug_detector.URIErrorArg
import kr.ac.kaist.jsaf.nodes_util.NodeUtil
import kr.ac.kaist.jsaf.utils.uri.URIHandling

object BuiltinGlobal extends ModelData {
  val quiet =
    if(Shell.params.command == ShellParameters.CMD_WEBAPP_BUG_DETECTOR)
      true
    else false

  //val GlobalLoc = newPreDefLoc("Global", Recent)

  private val prop_global: List[(String, AbsProperty)] = List(
    ("@class",             AbsConstValue(PropValue(AbsString.alpha("Object")))), // implementation dependent
    ("@proto",             AbsConstValue(PropValue(ObjectValue(ObjProtoLoc, F, F, F)))), // implementation dependent
    ("@extensible",        AbsConstValue(PropValue(BoolTrue))),
    // 15.1.1 Value Properties of the Global Object
    ("NaN",                AbsConstValue(PropValue(ObjectValue(NaN, F, F, F)))),
    ("Infinity",           AbsConstValue(PropValue(ObjectValue(PosInf, F, F, F)))),
    ("undefined",          AbsConstValue(PropValue(ObjectValue(UndefTop, F, F, F)))),
    // 15.1.2 Function Properties of the Global Object
    ("eval",               AbsBuiltinFunc("Global.eval", 1)),
    ("parseInt",           AbsBuiltinFunc("Global.parseInt", 2)),
    ("parseFloat",         AbsBuiltinFunc("Global.parseFloat", 1)),
    ("isNaN",              AbsBuiltinFunc("Global.isNaN", 1)),
    ("isFinite",           AbsBuiltinFunc("Global.isFinite", 1)),
    ("escape",             AbsBuiltinFunc("Global.escape", 1)),
    ("unescape",           AbsBuiltinFunc("Global.unescape", 1)),
    // 15.1.3 URI Handling Function Properties
    ("decodeURI",          AbsBuiltinFunc("Global.decodeURI", 1)),
    ("decodeURIComponent", AbsBuiltinFunc("Global.decodeURIComponent", 1)),
    ("encodeURI",          AbsBuiltinFunc("Global.encodeURI", 1)),
    ("encodeURIComponent", AbsBuiltinFunc("Global.encodeURIComponent", 1)),
    // builtin objects
    ("Array",              AbsConstValue(PropValue(ObjectValue(BuiltinArray.ConstLoc, T, F, T)))),
    ("Boolean",            AbsConstValue(PropValue(ObjectValue(BuiltinBoolean.ConstLoc, T, F, T)))),
    ("Date",               AbsConstValue(PropValue(ObjectValue(BuiltinDate.ConstLoc, T, F, T)))),
    ("Error",              AbsConstValue(PropValue(ObjectValue(BuiltinError.ErrConstLoc, T, F, T)))),
    ("EvalError",          AbsConstValue(PropValue(ObjectValue(BuiltinError.EvalErrConstLoc, T, F, T)))),
    ("RangeError",         AbsConstValue(PropValue(ObjectValue(BuiltinError.RangeErrConstLoc, T, F, T)))),
    ("ReferenceError",     AbsConstValue(PropValue(ObjectValue(BuiltinError.RefErrConstLoc, T, F, T)))),
    ("SyntaxError",        AbsConstValue(PropValue(ObjectValue(BuiltinError.SyntaxErrConstLoc, T, F, T)))),
    ("TypeError",          AbsConstValue(PropValue(ObjectValue(BuiltinError.TypeErrConstLoc, T, F, T)))),
    ("URIError",           AbsConstValue(PropValue(ObjectValue(BuiltinError.URIErrConstLoc, T, F, T)))),
    ("Function",           AbsConstValue(PropValue(ObjectValue(BuiltinFunction.ConstLoc, T, F, T)))),
    ("JSON",               AbsConstValue(PropValue(ObjectValue(BuiltinJSON.ConstLoc, T, F, T)))),
    ("Math",               AbsConstValue(PropValue(ObjectValue(BuiltinMath.ConstLoc, T, F, T)))),
    ("Number",             AbsConstValue(PropValue(ObjectValue(BuiltinNumber.ConstLoc, T, F, T)))),
    ("Object",             AbsConstValue(PropValue(ObjectValue(BuiltinObject.ConstLoc, T, F, T)))),
    ("RegExp",             AbsConstValue(PropValue(ObjectValue(BuiltinRegExp.ConstLoc, T, F, T)))),
    ("String",             AbsConstValue(PropValue(ObjectValue(BuiltinString.ConstLoc, T, F, T)))),
    // predefined constant variables from IR
    (NodeUtil.varTrue, AbsConstValue(PropValue(ObjectValue(BoolTrue, F, F, F)))),
    (NodeUtil.varOne, AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1.0), F, F, F)))),
    (NodeUtil.freshGlobalName("global"), AbsConstValue(PropValue(ObjectValue(Value(GlobalSingleton), F, F, F))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List((GlobalLoc, prop_global))
  
  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      ("Global.eval" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if (!quiet) System.out.println("* Warning: the 'Global.eval' call is detected during analysis, analysis results may not be sound.")
          // arguments
          val argv = getArgValue(h, ctx, args, "0")
          if(argv._1._5</StrBot){
            val s = Helper.toString(Helper.toPrimitive_better(h, argv))
            val message = s.gamma match {
              case Some(_) => s.toString
              case _ => s.getAbsCase match {
                case AbsTop => "StrTop"
                case AbsBot => "StrBot"
                case _ if s.isAllNums => "NumStr"
                case _ => "OtherStr"
              }
            }
            if (!quiet) System.out.println("* Warning : the argument of 'Global.eval' is in the below ...")           
            if (!quiet) System.out.println(message)
            // unsound
            ((h, ctx), (he, ctxe))
          }
          else {
            if (!quiet) System.out.println("* Warning: the argument of 'Global.eval' is a non-string")           
            if (!quiet) System.out.println("* The argument is " + DomainPrinter.printValue(argv))           
            ((Helper.ReturnStore(h, argv), ctx), (he, ctxe))
          }
        })
      ),

      ("Global.parseInt" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 15.1.2.2 parseInt(string, radix)
          val v_1 = getArgValue(h, ctx, args, "0") /* string */
          val v_2 = getArgValue(h, ctx, args, "1") /* radix */

          val inputString = Helper.toString(Helper.toPrimitive_better(h, v_1))
          // TODO: Simple implementation. Must be revised. Not the same as the original.
          val r = Operator.ToInt32(v_2)

          val value = Operator.parseInt(inputString, r)
          val rtn = Value(value)

          ((Helper.ReturnStore(h, rtn), ctx), (he, ctx))
        })
      ),

      ("Global.parseFloat" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // 15.1.2.2 parseInt(string, radix)
          val v_1 = getArgValue(h, ctx, args, "0") /* string */

          val inputString = Helper.toString(Helper.toPrimitive(v_1))
          // TODO: Simple implementation. Must be revised. Not the same as the original.

          val value = Operator.parseFloat(inputString)
          val rtn = Value(value)

          ((Helper.ReturnStore(h, rtn), ctx), (he, ctx))
        })
        ),

      "Global.decodeURI" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val lset_callee = getArgValue(h, ctx, args, "callee")._2
          val abstraction = (lset_callee.size > 1)
          val (value, es): (Value, Set[Exception]) = s.gamma match {
            case Some(vs) => {
              vs.foldLeft((ValueBot, ExceptionBot))((r, string) => {
                val rtn = URIHandling.decode(string, URIHandling.decodeURIString)
                val (v_1, es_1) =
                  if (rtn != null) {
                    (Value(AbsString.alpha(rtn)), ExceptionBot)
                  } else {
                    if (Config.typingInterface != null)
                      if(Shell.params.opt_DeveloperMode || !abstraction)
                        Config.typingInterface.signal(Config.typingInterface.getSpan, URIErrorArg, "Global.decodeURI", string)
                    (ValueBot, Set[Exception](URIError))
                  }

                (r._1 + v_1, r._2 ++ es_1)
              })
            }
            case None =>
              (Value(StrTop), Set[Exception](URIError))
          }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_1, ctx_1) =
            if (value </ ValueBot) {
              (Helper.ReturnStore(h, value), ctx)
            } else {
              (HeapBot, ContextBot)
            }

          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        }),

      "Global.decodeURIComponent" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))

          val lset_callee = getArgValue(h, ctx, args, "callee")._2
          val abstraction = (lset_callee.size > 1)
          
          val (value, es): (Value, Set[Exception]) = s.gamma match {
            case Some(vs) => {
              vs.foldLeft((ValueBot, ExceptionBot))((r, string) => {
                val rtn = URIHandling.decode(string, URIHandling.decodeURIComponentString)
                val (v_1, es_1) =
                  if (rtn != null) {
                    (Value(AbsString.alpha(rtn)), ExceptionBot)
                  } else {
                    if (Config.typingInterface != null)
                      if(Shell.params.opt_DeveloperMode || !abstraction)
                        Config.typingInterface.signal(Config.typingInterface.getSpan, URIErrorArg, "Global.decodeURIComponent", string)
                    (ValueBot, Set[Exception](URIError))
                  }

                (r._1 + v_1, r._2 ++ es_1)
              })
            }
            case None => (Value(StrTop), Set[Exception](URIError))
          }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_1, ctx_1) =
            if (value </ ValueBot) {
              (Helper.ReturnStore(h, value), ctx)
            } else {
              (HeapBot, ContextBot)
            }

          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        }),

      "Global.encodeURI" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          
          val lset_callee = getArgValue(h, ctx, args, "callee")._2
          val abstraction = (lset_callee.size > 1)

          val (value, es): (Value, Set[Exception]) = s.gamma match {
            case Some(vs) => {
              vs.foldLeft((ValueBot, ExceptionBot))((r, string) => {
                val rtn = URIHandling.encode(string, URIHandling.encodeURIString)
                val (v_1, es_1) =
                  if (rtn != null) {
                    (Value(AbsString.alpha(rtn)), ExceptionBot)
                  } else {
                    if (Config.typingInterface != null)
                      if(Shell.params.opt_DeveloperMode || !abstraction)
                        Config.typingInterface.signal(Config.typingInterface.getSpan, URIErrorArg, "Global.encodeURI", string)
                    (ValueBot, Set[Exception](URIError))
                  }

                (r._1 + v_1, r._2 ++ es_1)
              })
            }
            case None => (Value(StrTop), Set[Exception](URIError))
          }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_1, ctx_1) =
            if (value </ ValueBot) {
              (Helper.ReturnStore(h, value), ctx)
            } else {
              (HeapBot, ContextBot)
            }

          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        }),

      "Global.encodeURIComponent" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val s = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val lset_callee = getArgValue(h, ctx, args, "callee")._2
          val abstraction = (lset_callee.size > 1)

          val (value, es): (Value, Set[Exception]) = s.gamma match {
            case Some(vs) => {
              vs.foldLeft((ValueBot, ExceptionBot))((r, string) => {
                val rtn = URIHandling.encode(string, URIHandling.encodeURIComponentString)
                val (v_1, es_1) =
                  if (rtn != null) {
                    (Value(AbsString.alpha(rtn)), ExceptionBot)
                  } else {
                    if (Config.typingInterface != null)
                      if(Shell.params.opt_DeveloperMode || !abstraction)
                        Config.typingInterface.signal(Config.typingInterface.getSpan, URIErrorArg, "Global.encodeURIComponent", string)
                    (ValueBot, Set[Exception](URIError))
                  }

                (r._1 + v_1, r._2 ++ es_1)
              })
            }
            case None => (Value(StrTop), Set[Exception](URIError))
          }
          val (h_e, ctx_e) = Helper.RaiseException(h, ctx, es)
          val (h_1, ctx_1) =
            if (value </ ValueBot) {
              (Helper.ReturnStore(h, value), ctx)
            } else {
              (HeapBot, ContextBot)
            }

          ((h_1, ctx_1), (he + h_e, ctxe + ctx_e))
        }),

      ("Global.isNaN" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val n = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val b =
            if (NaN == n)
              BoolTrue
            else if (NaN </ n)
              BoolFalse
            else if (NaN <= n)
              BoolTop
            else
              BoolBot
          ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
        })
      ),
      ("Global.isFinite" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val n = Helper.toNumber(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val b =
            if (NaN == n || PosInf == n || NegInf == n)
              BoolFalse
            else if (NaN </ n && PosInf </ n && NegInf </ n)
              BoolTrue
            else if (NaN <= n || PosInf <= n || NegInf <= n)
              BoolTop
            else
              BoolBot
          ((Helper.ReturnStore(h, Value(b)), ctx), (he, ctxe))
        })
      ),
      // ECMAScript B.2.1 escape (string)
      ("Global.escape" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          // 1. CallToString(string)
          val str = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (str </ StrBot){
            val encodedStr = str.getSingle match {
              case Some(v) =>
                val chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789@*_+-./"
                var S = ""
                // 2. Compute the number of characters in Result(1)
                val len = v.size
                // 3. Let R be the empty string.
                var R = ""
                // 4, Let k be 0
                var k = 0
                // 5, if k equals Result(2), return R
                while (k != len) {
                  // 6, Get the caracter at position k within Result(1)
                  val r6 = v.charAt(k)
                  // 7, If Result(6) is one of the 69 nonblack characters, then go to step 13
                  S = if(!chars.contains(r6)) {
                    // 8, If Resut (6) is less tan 256, go to step 11.
                    if(r6.toInt < 256) {
                      // 11, Let S be a String containing three characters "%xy" where sy are ...
                      val hex2 = r6.toInt.toHexString.take(2).toUpperCase
                      "%" + (if (hex2.size == 1) "0" + hex2 else hex2)
                      // 12, Go to step 14
                    }
                    else {
                      // 9, Let S be a String containing six characters “%uwxyz” where wxyz are four hexadecimal digits encoding the
                      // value of Result(6).
                      var hex4 = r6.toInt.toHexString.take(4).toUpperCase
                      while(hex4.size!=4)
                        hex4 = "0" + hex4
                      "%u" + hex4
                    }

                  }
                  else {
                    // 13, Let S be a String containing the single character Result(6)
                    "" + r6
                  }
                  // 14, Let R be a new String value computed by concatenating the previous value of R and S
                  R = R ++ S
                  k = k + 1
                }
                AbsString.alpha(R)
              case None => StrTop
            }
            ((Helper.ReturnStore(h, Value(encodedStr)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))

        })),
      // ECMAScript B.2.2 unescape (string)
      ("Global.unescape" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          // 1. Call ToString(string).
          val aString = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (aString </ StrBot) {
            val unescapedString = aString.getSingle match {
              case Some(str) =>
                // 2. Compute the number of characters in Result(1).
                val len = str.length
                // 3. Let R be the empty String.
                val R = new StringBuffer
                // 4. Let k be 0.
                var k = 0
                while (k < len) {
                  // 6. Let c be the character at position k within Result(1).
                  val c = str.charAt(k)
                  var codepoint: Option[Int] = None
                  // 7. If c is not %, go to step 18.
                  if (c == '%') {
                    val rest = str.substring(k).tail.take(5)
                    // 8. If k is greater than Result(2)−6, go to step 14.
                    // 9. If the character at position k+1 within Result(1) is not u, go to step 14.
                    if (k <= len - 6 && rest.head == 'u') {
                      // 10. If the four characters at positions k+2, k+3, k+4, and k+5 within Result(1) are not all
                      // hexadecimal digits, go to step 14.
                      try {
                        // 11. Let c be the character whose code unit value is the integer represented by the four
                        // hexadecimal digits at positions k+2, k+3, k+4, and k+5 within Result(1).
                        codepoint = Some(Integer.valueOf(rest.tail.mkString, 16))
                        // 12. Increase k by 5.
                        k += 5
                        // 13. Go to step 18.
                      } catch {
                        case nfe: NumberFormatException => ()
                      }
                    }
                    // 14. If k is greater than Result(2)−3, go to step 18.
                    if (codepoint.isEmpty && k <= len - 3) {
                      // 15. If the two characters at positions k+1 and k+2 within Result(1) are not both hexadecimal
                      // digits, go to step 18.
                      try {
                        // 16. Let c be the character whose code unit value is the integer represented by two zeroes
                        // plus the two hexadecimal digits at positions k+1 and k+2 within Result(1).
                        codepoint = Some(Integer.valueOf(rest.take(2).mkString, 16))
                        // 17. Increase k by 2.
                        k += 2
                      } catch {
                        case nfe: NumberFormatException => ()
                      }
                    }
                  }

                  // 18. Let R be a new String value computed by concatenating the previous value of R and c.
                  codepoint match {
                    case Some(cp) => R.appendCodePoint(cp)
                    case None => R.append(c)
                  }

                  // 19. Increase k by 1.
                  k += 1
                }
                // 5. If k equals Result(2), return R.
                AbsString.alpha(R.toString)
              case None => StrTop
            }
            ((Helper.ReturnStore(h, Value(unescapedString)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("Global.alert" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
        })
      )
    )
  }

}
