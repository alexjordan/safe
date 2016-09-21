/*******************************************************************************
    Copyright (c) 2013-2014, KAIST.
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

package kr.ac.kaist.jsaf.bug_detector

import scala.collection.mutable.{ HashMap => MHashMap, ListBuffer => MListBuffer }
import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.CallContext._
import kr.ac.kaist.jsaf.analysis.typing.{ SemanticsExpr => SE }
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.models.ModelManager
import kr.ac.kaist.jsaf.ts.{TSTypeMap, TSToString}
import kr.ac.kaist.jsaf.nodes._
import kr.ac.kaist.jsaf.nodes_util.{ NodeRelation, Span }
import kr.ac.kaist.jsaf.scala_src.nodes._
import kr.ac.kaist.jsaf.scala_src.useful.Lists._
import java.util.{List => JList}

class TSChecker(bugDetector: BugDetector)  extends Walker {
  val cfg           = bugDetector.cfg
  val typing        = bugDetector.typing
  val semantics     = bugDetector.semantics
  val stateManager  = bugDetector.stateManager

  val bugStorage    = bugDetector.bugStorage
  val bugOption     = bugDetector.bugOption

  var cstate: CState = Map[CallContext, State]()

  def properArgSize (argSize: (Int, Int), num: Int): Option[String] = {
    if (argSize._2 == -1 && argSize._1 > num) {
      Some("equal or more than "+argSize._1)
    } else if (argSize._2 != -1 && (num < argSize._1 || num > argSize._2)) {
      if (argSize._1 == argSize._2)
        Some(argSize._1.toString)
      else
        Some(argSize._1+" to "+argSize._2)
    } else {
      None
    }
  }

  def checkArgsWithFuncs(_cstate: CState, span: Span, name: String, args: List[Value]): Unit = {
    var msgs: List[() => Unit] = List[() => Unit]()
    var error: Boolean = true
    val funListOpt:Option[List[Type]] = TSTypeMap.intfFunMap.get(name) match {
      case f@Some(_) => f
      case None => TSTypeMap.globalFunMap.get(name) match {
        case f@Some(_) => f
        case None => TSTypeMap.callMap.get(name) match {
          case f@Some(_) => f
          case None => None
        }
      }
    }

    funListOpt match {
      case Some(funList) => funList.foreach( funNode =>
        if (error) {
          funNode match {
            case SCallSig(info, tparams, params, typ) =>{
              cstate = _cstate
              var result: List[() => Unit] = checkArgs(span, funNode, name, params, args)
              if(result.length<1) error = false
              else msgs ++= result
            }
            case SFunctionType(info, tparams, params, typ) =>{
              cstate = _cstate
              var result: List[() => Unit] = checkArgs(span, funNode, name, params, args)
              if(result.length<1) error = false
              else msgs ++= result
            }
            case _ =>
          }
        })
      case None =>
    }
    if (error) {
      msgs.foreach(msg => msg())
    }
  }

  def checkArgs(span: Span, fa: Type, name: String, params: List[Param], args: List[Value])
    : List[() => Unit] = {
    var argEnvs: List[Pair[Obj, State]] = List()
    var msgs: List[() => Unit] = List()
    var numOfOptional: Int = 0
    var numOfParam: Int = params.size
    var numOfArgument: Int = args.length
    var hasRest: Boolean = false

    var argSize: (Int, Int) = (-1, -1)

    for (i <- 0 until params.size) params.get(i) match {
      case SParam(_, _, _, typ, _, opt, rest) => {
        if (rest) {
          hasRest = true
          if (i < numOfArgument) {
            typ match {
              case Some(t) => {
                t match {
                  case SArrayType(info, _t) => {
                    for (j <- i until numOfArgument) {
                      val arg = args.get(j)
                      if(!checkType(arg, _t)) {
                        msgs :+= (() => { bugStorage.addMessage(span, TSWrongArgType, null, null, "s #"+(i+1)+" and after that", name, _t) })
                      }
                    }
                  }
                  case _ =>
                }
              }
              case None =>
            }
          }
        } else {
          if (i < numOfArgument) {
            typ match {
              case Some(t) => {
                val arg = args.get(i)
                if(!checkType(arg, t)) {
                  msgs :+= (() => { bugStorage.addMessage(span, TSWrongArgType, null, null, " #"+(i+1), name, t) })
                }
              }
              case None =>
            }
          }
        }
        if (opt) {
          numOfOptional+=1
        }
      }
    }

    if (!hasRest) {
      argSize = (numOfParam-numOfOptional, numOfParam)
    } else {
      argSize = (numOfParam-numOfOptional-1, -1)
    }

    properArgSize(argSize, numOfArgument) match {
      case Some(str) => {
        msgs :+= (() => { bugStorage.addMessage(span, TSWrongArgs, null, null, name, str, numOfArgument) })
      }
      case None =>
    }
    msgs
  }

  def isAbsent(value: Value): Boolean = {
    ((value.pv.undefval.isBottom)
    && (value.pv.nullval.isBottom)
    && (value.pv.boolval.isBottom)
    && (value.pv.strval.isBottom)
    && (value.pv.numval.isBottom)
    && (value.locs.size==0))
  }

  def checkType(given: Value, expected: Type): Boolean = { 
    val state = typing.mergeState(cstate)

    val undefval = given.pv.undefval
    val nullval = given.pv.nullval
    val boolval = given.pv.boolval
    val strval = given.pv.strval
    val numval = given.pv.numval
    val locset = given.locs

    expected match {
      case SAnyT(info) => true
      case SNumberT(info) => !numval.isBottom
      case SStringT(info) => !strval.isBottom
      case SBoolT(info) => !boolval.isBottom
      case SVoidT(info) => !undefval.isBottom
      case SFunctionType(info, tparams, params, typ) => {
        for (funLoc <- locset) {
          val propertyName: String = BugHelper.getFuncOrConstPropName(state.heap, funLoc, true)
          if (propertyName != null) {
            for (fid <- state.heap(funLoc)(propertyName).funid) {
              val plen = params.length
              val alen = state.heap(funLoc)("length").objval.value.pv.numval.getSingle match {
                case Some(v) => v
                case None => -1
              }
              if (plen == alen){
                return true
              }
            }
          }
        }
        false
      }
      case SArrayType(info, typ) => {
        for (arrLoc <- locset) {
          Helper.IsArray(state.heap, arrLoc).getSingle match {
            case Some(b) => {
              if (b) {
                val arr = state.heap(arrLoc)
                val length = arr("length").objval.value.pv.numval.getSingle match {
                  case Some(v) => v.toInt
                  case None => -1
                }
                for (i <- 0 until length) {
                  if(!checkType(arr(i.toString).objval.value, typ)) {
                    return false
                  }
                }
                return true
              }
            }
            case None =>
          }
        }
        false
      }
      case SObjectType(info, mems) => {
        for (objLoc <- locset) {
          Helper.IsObject(state.heap, objLoc).getSingle match {
            case Some(b) => {
              if (b) {
                val obj = state.heap(objLoc)
                for (mem <- mems) mem match {
                  case mem@SPropertySig (info, prop, opt, typ@Some(t)) => {
                    val name: String = prop match {
                      case SPropId(info,id) => id.getText
                    }
                    if(isAbsent(obj(name).objval.value)) {
                      if(!opt) {
                        return false
                      }
                    } else {
                      if(!checkType(obj(name).objval.value, t)) {
                        return false
                      }
                    }
                  }
                  case mem@SMethodSig(info, prop, opt, sig) => {
                    val name: String = prop match {
                      case SPropId(info,id) => id.getText
                    }
                    if(isAbsent(obj(name).objval.value)){
                      if(!opt) {
                        return false
                      }
                    } else {
                      if(!checkType(obj(name).objval.value, sig)) {
                        return false
                      }
                    }
                  }
                  case mem@SConstructSig (info, tparams, params, typ) => {
                  }
                  case _ =>
                }
                return true
              }
            }
            case None =>
          }
        }
        false
      }
      case SCallSig(info, tparams, params, typ) => {
        for (funLoc <- locset) {
          val propertyName: String = BugHelper.getFuncOrConstPropName(state.heap, funLoc, true)
          if (propertyName != null) {
            for (fid <- state.heap(funLoc)(propertyName).funid) {
              val plen = params.length
              val alen = state.heap(funLoc)("length").objval.value.pv.numval.getSingle match {
                case Some(v) => v
                case None => -1
              }
              if (plen == alen){
                return true
              }
            }
          }
        }
        false
      }
      case typ@STypeRef(info, name, args) => {
        for (objLoc <- locset) {
          Helper.IsObject(state.heap, objLoc).getSingle match {
            case Some(b) => {
              if (b) {
                if (checkBuiltIn(name.getText)){
                  return true
                }
                TSTypeMap.intfMap(name.getText) match {
                  case node@SIntfDecl(_, _, _, _, ty@SObjectType(_, mems)) => {
                    val obj = state.heap(objLoc)
                    for (mem <- mems) mem match {
                      case mem@SPropertySig (info, prop, opt, typ@Some(t)) => {
                        val name: String = prop match {
                          case SPropId(info,id) => id.getText
                        }
                        if(isAbsent(obj(name).objval.value)) {
                          if(!opt) {
                            return false
                          }
                        } else {
                          if(!checkType(obj(name).objval.value, t)) {
                            return false
                          }
                        }
                      }
                      case mem@SMethodSig(info, prop, opt, sig) => {
                        val name: String = prop match {
                          case SPropId(info,id) => id.getText
                        }
                        if(isAbsent(obj(name).objval.value)) {
                          if(!opt) {
                            return false
                          }
                        } else {
                          if(!checkType(obj(name).objval.value, sig)) {
                            return false
                          }
                        }
                      }
                      case _ =>
                    }
                    return true
                  }
                }
              }
            }
            case None =>
          }
        }
        false
      }
      case _ => false
    }
  }
  def checkBuiltIn(name: String): Boolean = {
    name match {
      case "Function" => true
      case "HTMLElement" => true
      case "Object" => true
      case "Element" => true
      case "Text" => true
      case "Node" => true
      case "T" => true
      case "XMLDocument" => true
      case _ => false
    }
  }
}
