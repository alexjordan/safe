/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ***************************************************************************** */
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

package kr.ac.kaist.jsaf.analysis.typing

import scala.collection.immutable.HashMap
import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.{SemanticsExpr => SE}
import kr.ac.kaist.jsaf.analysis.typing.models.builtin.BuiltinError
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.analysis.typing.models.ModelManager
import kr.ac.kaist.jsaf.nodes_util.{Span, NodeUtil}

class Statistics(cfg: CFG, builtins:Map[FunctionId, String], inTable: Table, locclone: Boolean) {
  val sem = new Semantics(cfg, Worklist.computes(cfg), locclone)
  val globalStat = new Stat(sem, cfg, builtins, inTable)
  
  def setDerefWithoutHeap(flag: Boolean) = globalStat.setDerefWithoutHeap(flag)
  def printDump() = {
    globalStat.printDump();
  }
  
  def printTable() = {
    globalStat.printTable();
  }

  var activeBB = Set[Node]()
  var activeExcBB = Set[Node]()
  def dumpActiveBB = {
    println("=== dump activeBB === size : " + (activeBB ++ activeExcBB).size)
    activeBB.foreach(println)
    activeExcBB.foreach(println)
    println("=====================")
  }
  def dumpCoveredBB = {
    println("=== dump coveredBB === size : " + (globalStat.coveredBB).size)
    globalStat.coveredBB.toList.sortBy(node => {node._1}).foreach(println)
    println("=====================")
  }
  def calculate(fidset: Set[FunctionId] = null) = {
    def C(node: Node, map: CState, c: Cmd, nodeStat: Stat): Unit = {
      /* for after-call */
      cfg.getReturnVar(node) match {
        case None => Unit
        case Some(id) => if(fidset == null || fidset.contains(node._1)) nodeStat.countVarUpdate(map, id)
      }
      
      c match {
        case Entry =>
          if (map.forall((kv) => kv._2._1 <= HeapBot)) {
            nodeStat.deadBB = nodeStat.deadBB + 1;
          }
          else {
            activeBB += node
          }
        case Exit =>
          if (map.forall((kv) => kv._2._1 <= HeapBot)) {
            nodeStat.deadBB = nodeStat.deadBB + 1;
          }
          else {
            activeBB += node
          }
        case ExitExc =>
          if (map.forall((kv) => kv._2._1 <= HeapBot)) {
          }
          else {
            activeExcBB += node
          }
        case Block(insts) => {
          if (map.forall((kv) => kv._2._1 <= HeapBot)) {
            nodeStat.deadBB = nodeStat.deadBB + 1;
          }
          else {
            activeBB += node
          }
          if(fidset == null || fidset.contains(node._1)) {
            insts.foldLeft[CState](map)((stateMap, inst) => {
              // count dead instruction
              val isDead = stateMap.exists(
                (kv) => (kv._2._1 <= HeapBot && !builtins.contains(node._1)))
              if (isDead)
                  nodeStat.deadInst = nodeStat.deadInst + 1
              else {
                activeBB += node
              }
              
              // compute next normal states and exception state using semantics
              val (cs, es) = stateMap.foldLeft[(CState,State)]((HashMap(),StateBot))((ms, kv) => {
                val ((h, ctx), (he, ctxe)) = sem.I((node, kv._1), inst, kv._2.heap, kv._2.context, HeapBot, ContextBot)
                (ms._1 + (kv._1 -> State(h, ctx)), ms._2 + State(he, ctxe))
              })
              
              // compute other statistics
              //val before_size = nodeStat.absentVarSet.size
              I(node, inst, stateMap, cs, nodeStat) 
              //val after_size = nodeStat.absentVarSet.size
              //if(before_size != after_size)
              //  println("node : " +node)

              // count exception
              if (!(es._1 <= HeapBot)) {
                globalStat.countException(es._1(SinglePureLocalLoc)("@exception")._2)
              }
              
              // normal CState for next instruction
              cs
            })
          }
          Unit
        }
      }
    }
    def I(node: Node, i: CFGInst, stateMap: CState, nextStateMap: CState, nodeStat:Stat) = {
      nodeStat.derefInst = i
        i match {
          case CFGAlloc(_, _ , x, e, a_new) => {
            nodeStat.countVarUpdate(stateMap, x)
            e match {
              case None => () 
              case Some(expr) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
                nodeStat.countDeref(stateMap, nextStateMap, expr, "Alloc")
              }
            }
          }
          case CFGAllocArray(_, _, x, n, a_new) => {
            nodeStat.countVarUpdate(stateMap, x)
          }
          case CFGAllocArg(_, _, x, n, a_new) => {
            nodeStat.countVarUpdate(stateMap, x)
          }
          case CFGExprStmt(_, _,x, expr) => {
            V(node, expr, stateMap, nextStateMap, nodeStat);
            nodeStat.countVarUpdate(stateMap, x)
          }
          case CFGDelete(_, _, lhs, expr) => {
            V(node, expr, stateMap, nextStateMap, nodeStat);
            nodeStat.countVarUpdate(stateMap, lhs)
          }
          case CFGDeleteProp(_, _, lhs, obj, index) => {
            V(node, obj, stateMap, nextStateMap, nodeStat);
            V(node, index, stateMap, nextStateMap, nodeStat);
            nodeStat.countVarUpdate(stateMap, lhs)
            nodeStat.countDeref(stateMap, nextStateMap, obj, "Delete")
            nodeStat.countAccess(stateMap, index, "Delete")
          }
          case CFGStore(_, _, obj, index, rhs) => {
            V(node, obj, stateMap, nextStateMap, nodeStat);
            V(node, index, stateMap, nextStateMap, nodeStat);
            V(node, rhs, stateMap, nextStateMap, nodeStat);
            nodeStat.countDeref(stateMap, nextStateMap, obj, "Store")
            nodeStat.countAccess(stateMap, index, "Store")
            nodeStat.countPropUpdate(stateMap, obj)
          }
          case CFGFunExpr(_, _, lhs, name, fid, a_new1, a_new2, a_new3) => {
            nodeStat.countVarUpdate(stateMap, lhs)
          }
          case CFGConstruct(_, _, cons, base, arguments, a_new, b_new) => {
            V(node, cons, stateMap, nextStateMap, nodeStat)
            V(node, base, stateMap, nextStateMap, nodeStat)
            V(node, arguments, stateMap, nextStateMap, nodeStat)
            nodeStat.countDeref(stateMap, nextStateMap, cons, "New(cons)")
            nodeStat.countDeref(stateMap, nextStateMap, base, "New(base)")
            nodeStat.countDefNew(stateMap, cons)
            nodeStat.countCall(stateMap, i, "CFGConstruct")
          }
          case CFGCall(_, _, fun, base, arguments, a_new, b_new) => {
            V(node, fun, stateMap, nextStateMap, nodeStat)
            V(node, base, stateMap, nextStateMap, nodeStat)
            V(node, arguments, stateMap, nextStateMap, nodeStat)
            nodeStat.countDeref(stateMap, nextStateMap, fun, "Call(fun)")
            nodeStat.countDeref(stateMap, nextStateMap, base, "Call(base)")
            nodeStat.countDefCall(stateMap, fun)
            nodeStat.countCall(stateMap, i, "CFGCall")
          }
          case CFGCatch(_, _, name) => {
            nodeStat.countVarUpdate(stateMap, name)
          }
          case CFGReturn(_, _, expr) => {
            expr match {
              case None => Unit
              case Some(e) => V(node, e, stateMap, nextStateMap, nodeStat)
            }
          }
          case CFGAssert(_, _, expr, flag) => {
            if(flag) V(node, expr, stateMap, nextStateMap, nodeStat)
          }
          case CFGThrow(_, _, expr) => {
            V(node, expr, stateMap, nextStateMap, nodeStat)
          }
          case CFGInternalCall(_, info, lhs, fun, arguments, loc) => {
            nodeStat.countVarUpdate(stateMap, lhs)
            (fun.toString, arguments, loc)  match {
              case ("<>Global<>toObject", List(expr), Some(a_new)) => { 
                V(node, expr, stateMap, nextStateMap, nodeStat)
                nodeStat.countType(stateMap, expr, "Object")

                val filename = info.getSpan.begin.getFileName
              }
              case ("<>Global<>isObject", List(expr), None) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
                stateMap.foreach((kv)=> {
                  if (!(kv._2._1 <= HeapBot)) {
                    val v = SE.V(expr, kv._2._1, kv._2._2)._1
                  }})
              }
//              case ("<>Global<>toString", List(expr), None) => {
//                V(expr, stateMap, nextStateMap, nodeStat)
//                nodeStat.countType(stateMap, expr, "String")
//              }
              case ("<>Global<>toNumber", List(expr), None) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
                nodeStat.countType(stateMap, expr, "Number")
              }
              case ("<>Global<>toBoolean", List(expr), None) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
                nodeStat.countType(stateMap, expr, "Bool")
              }
              case ("<>Global<>getBase", List(expr), None) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
                val filename = info.getSpan.begin.getFileName

                //if(!(filename.endsWith("dOMEvent") || filename.endsWith("__builtin__.js") || filename.endsWith("__dom__.js") || filename.endsWith("__input__.js"))){
                  //val old = nodeStat.impreciseCallSet.size 
                  //nodeStat.countImpreciseCall(node, info.getSpan, stateMap, expr)
                  //val newsize = nodeStat.impreciseCallSet.size 
                  //if(old!=newsize) 
                  //  println("getbase node : " + node)
                //}


              }
              case ("<>Global<>iteratorInit", List(expr), None) => {
                V(node, expr, stateMap, nextStateMap, nodeStat)
              }
              case ("<>Global<>iteratorHasNext", List(expr_2, expr_3), None) => {
                V(node, expr_2, stateMap, nextStateMap, nodeStat)
                V(node, expr_3, stateMap, nextStateMap, nodeStat)
              }
              case ("<>Global<>iteratorNext", List(expr_2, expr_3), None) => {
                V(node, expr_2, stateMap, nextStateMap, nodeStat)
                V(node, expr_3, stateMap, nextStateMap, nodeStat)
              }
              case _ => ()
            }
        }
        case CFGAPICall(_, _, "Function.prototype.apply", args) =>
          // this value is for callees
          nodeStat.countCall(stateMap, i, "APIApply")
        case CFGAPICall(_, _, "Function.prototype.call", args) => 
          nodeStat.countCall(stateMap, i, "APICall")
        case _ => ()
      }
    }
    
    def V(node: Node, e: CFGExpr, stateMap: CState, nextStateMap: CState, nodeStat:Stat, istypeof:Boolean =false): Unit= {
      e match {
        case CFGBin(info, first, op, second) => {
          V(node, first, stateMap, nextStateMap, nodeStat)
          V(node, second, stateMap, nextStateMap, nodeStat)
          op.getText match {
            case "instanceof" => {
              nodeStat.countDeref(stateMap, nextStateMap, first, "ins(obj)")
              nodeStat.countDeref(stateMap, nextStateMap, second, "ins(cons)")
            }
            case "in" => {
              nodeStat.countDeref(stateMap, nextStateMap, second, "in")
            }
            case _ => Unit
          }
        }
        case CFGUn(info, op, expr) => {
          if(op.getText == "typeof")
            V(node, expr, stateMap, nextStateMap, nodeStat, true)
          else V(node, expr, stateMap, nextStateMap, nodeStat)
        }
        case CFGLoad(info, obj, index) => {
          V(node, obj, stateMap, nextStateMap, nodeStat)
          V(node, index, stateMap, nextStateMap, nodeStat)
          nodeStat.countDeref(stateMap, nextStateMap, obj, "Load")
          nodeStat.countAccess(stateMap, index, "Load")
        }
        case CFGThis(info) => {
          nodeStat.countDeref(stateMap, nextStateMap, e, "this")
        }
        case CFGVarRef(info, id) =>
          if(!istypeof)
            nodeStat.countAbsentVar(node, stateMap, info.getSpan, id)
        case _ => Unit
      }
    }
    
    globalStat.totalBB = cfg.getNodes.size
    var dead = 0
    var active = 0
    cfg.getNodes.foreach((node) => {
      inTable.get(node) match {
        case None => {
          globalStat.deadBB = globalStat.deadBB + 1;
          dead += 1
          cfg.getCmd(node) match {
            case Block(insts) =>{
              if (!builtins.contains(node._1))
                globalStat.deadInst = globalStat.deadInst + insts.length;
            }
            case _ => Unit
          }
        }
        case Some(map) =>
          active += 1
          C(node, map, cfg.getCmd(node), globalStat)
      }
    })
  }
}


class Stat(sem: Semantics, cfg: CFG, builtins: Map[FunctionId, String], inTable: Table) {
  /* dereference type */
  type DerefStat = (Int, Int, Int, Int) 
  // _1 => total deref. (node)
  // _2 => total deref. node point (node)
  // _3 => maximum deref. (cc)
  // _4 => num. of definite deref (cc)
  
  /* dereference: total */
  var deref: DerefStat = (0,0,0,0)
  /* dereference: Load */
  var derefLoad: DerefStat = (0,0,0,0)
  /* dereference: Store */
  var derefStore: DerefStat = (0,0,0,0)
  /* dereference: DeleteProp */
  var derefDel: DerefStat = (0,0,0,0)
  /* dereference: Call Function, Base */
  var derefCallFun: DerefStat = (0,0,0,0)
  var derefCallBase: DerefStat = (0,0,0,0)
  /* dereference: New Constructor, Base */
  var derefNewCons: DerefStat = (0,0,0,0)
  var derefNewBase: DerefStat = (0,0,0,0)
  /* dereference: Alloc */
  var derefAlloc: DerefStat = (0,0,0,0)
  /* dereference: instanceof Object, Constructor*/
  var derefInsObj: DerefStat = (0,0,0,0)
  var derefInsCons: DerefStat = (0,0,0,0)
  /* dereference: in */
  var derefIn: DerefStat = (0,0,0,0)
  /* dereference: this */
  var derefThis: DerefStat = (0,0,0,0)
  /* dereference size from 0 to over 5 */
  val derefSizeCP = new Array[Int](7)
  val derefSizeNode = new Array[Int](7)


  
  /* dereference type */
  type CallableStat = ((Int, Int, Int, Int), (Int, Int, Int, Int))
  // _1._1 => callable sites point (node)
  // _1._2 => total callee function (node)
  // _1._3 => max callee function (node)
  // _1._4 => definite callsite (node)
  // _2._1 => callable sites point (cp)
  // _2._2 => total callee function (cp)
  // _2._3 => max callee function (cp)
  // _2._4 => definite callsite (cp)
  var callable: CallableStat = ((0,0,0,0),(0,0,0,0))
  var callableCFGCall: CallableStat = ((0,0,0,0),(0,0,0,0))
  var callableCFGConstruct: CallableStat = ((0,0,0,0),(0,0,0,0))
  var callableAPIApply: CallableStat = ((0,0,0,0),(0,0,0,0))
  var callableAPICall: CallableStat = ((0,0,0,0),(0,0,0,0))
  // 0-callee
  // 1-callee
  // 2-callee
  // 3-callee
  // more than 3-callee
  var callableCalleeSize = ((0,0,0,0,0),(0,0,0,0,0))
  var preCallGraph: Map[CFGInst, FunSet] = null
  
  /* definite function call/new */
  var callPoint = 0;
  var definiteFun = 0; 
  var newPoint = 0;
  var definiteCons = 0;
  
  /* type */
  type TypeStat = (Int, Int, Int, Int, Int) 
  // _1 => total type (node)
  // _2 => num. of undef (node)
  // _3 => total type conversion point (node)
  // _4 => maximum type (cc)
  // _5 => num. of definite type (cc)
  var typ: TypeStat = (0,0,0,0,0);
  var typBool: TypeStat = (0,0,0,0,0);
  var typNum: TypeStat = (0,0,0,0,0);
  var typStr: TypeStat = (0,0,0,0,0);
  var typObj: TypeStat = (0,0,0,0,0);
  
  /* access type */
  type AccessStat = (Int, Int)
  // _1 => fixed string access
  // _2 => access point
  var access: AccessStat = (0,0);
  var accessLoad: AccessStat = (0,0);
  var accessStore: AccessStat = (0,0);
  var accessDel: AccessStat = (0,0);
  
  /* strong update */
  var updatePoint = 0;
  var updateStrong = 0;
  
  /* dead code */
  var totalBB = 0;
  var deadBB = 0;
  var deadInst = 0;
  var coveredBBSize = 0;
  var coveredBB: Set[Node] = null;
  
  /* exception */
  var totalEx = 0;
  var errEx = 0;
  var evalEx = 0;
  var rangeEx = 0;
  var refEx = 0;
  var syntaxEx = 0;
  var typeEx = 0;
  var uriEx = 0;

  // LSA
  var usedFunctionSet: FunSet = FunSetBot;
  //var impreciseCallSet: Set[(Node,Span)] = Set();
  var absentVarSet: Set[(Node,Span)] = Set();
  //var nullUndefPropertyAccessSet: Set[(Node,Span)] = Set();

  var mcPerInst: Map[CFGInst, List[Int]] = Map()
  var mdPerExpr: Map[CFGExpr, List[Int]] = Map()

  def printTable() = {
    val derefAvg = 
      if (deref._2 > 0)  (deref._1 * 1.0d)/ deref._2
      else  0.0
    val derefLoadAvg = 
      if (derefLoad._2 > 0)  (derefLoad._1 * 1.0d)/ derefLoad._2
      else  0.0
    val derefStoreAvg = 
      if (derefStore._2 > 0)  (derefStore._1 * 1.0d)/ derefStore._2
      else  0.0
    val derefDelAvg = 
      if (derefDel._2 > 0)  (derefDel._1 * 1.0d)/ derefDel._2
      else  0.0
    val derefCallFunAvg = 
      if (derefCallFun._2 > 0)  (derefCallFun._1 * 1.0d)/ derefCallFun._2
      else  0.0
    val derefCallBaseAvg = 
      if (derefCallBase._2 > 0)  (derefCallBase._1 * 1.0d)/ derefCallBase._2
      else  0.0
    val derefNewConsAvg = 
      if (derefNewCons._2 > 0)  (derefNewCons._1 * 1.0d)/ derefNewCons._2
      else  0.0
    val derefNewBaseAvg = 
      if (derefNewBase._2 > 0)  (derefNewBase._1 * 1.0d)/ derefNewBase._2
      else  0.0
    val derefAllocAvg = 
      if (derefAlloc._2 > 0)  (derefAlloc._1 * 1.0d)/ derefAlloc._2
      else  0.0
    val derefInsObjAvg = 
      if (derefInsObj._2 > 0)  (derefInsObj._1 * 1.0d)/ derefInsObj._2
      else  0.0
    val derefInsConsAvg = 
      if (derefInsCons._2 > 0)  (derefInsCons._1 * 1.0d)/ derefInsCons._2
      else  0.0
    val derefInAvg = 
      if (derefIn._2 > 0)  (derefIn._1 * 1.0d)/ derefIn._2
      else  0.0
    val derefThisAvg = 
      if (derefThis._2 > 0)  (derefThis._1 * 1.0d)/ derefThis._2
      else  0.0
      
    val derefTable =  
"""================================= Dereference =================================
| dereference point(#)    : %8d                                          |
| average dereference(#)  : %8.2f                                          |
| maximum dereference(#)  : %8d                                          |
| definite deref. ratio(%%): %8.2f                                          |
|-----------+---Load---+---Store--+--Delete--+---Alloc--+---Call(fun, base)---|
|point   (#)| %8d | %8d | %8d | %8d | %8d | %8d |
|average (#)| %8.2f | %8.2f | %8.2f | %8.2f | %8.2f | %8.2f |
|maximum (#)| %8d | %8d | %8d | %8d | %8d | %8d |
|definite(%%)| %8.2f | %8.2f | %8.2f | %8.2f | %8.2f | %8.2f |
|-----------+---New(cons, base)---+-Instanceof(obj,con)-+----In----+---this---|
|point   (#)| %8d | %8d | %8d | %8d | %8d | %8d |
|average (#)| %8.2f | %8.2f | %8.2f | %8.2f | %8.2f | %8.2f |
|maximum (#)| %8d | %8d | %8d | %8d | %8d | %8d |
|definite(%%)| %8.2f | %8.2f | %8.2f | %8.2f | %8.2f | %8.2f |
===============================================================================
""".format(
        deref._2, derefAvg, deref._3, (deref._4 * 1.0d) / deref._2 * 100,
        derefLoad._2, derefStore._2, derefDel._2, derefAlloc._2, derefCallFun._2, derefCallBase._2,
        derefLoadAvg, derefStoreAvg, derefDelAvg, derefAllocAvg, derefCallFunAvg, derefCallBaseAvg,
        derefLoad._3, derefStore._3, derefDel._3, derefAlloc._3, derefCallFun._3, derefCallBase._3,
        
        (derefLoad._4 * 1.0d)    / derefLoad._2    * 100, (derefStore._4 * 1.0d)    / derefStore._2    * 100,
        (derefDel._4 * 1.0d)     / derefDel._2     * 100, (derefAlloc._4 * 1.0d)    / derefAlloc._2    * 100,
        (derefCallFun._4 * 1.0d) / derefCallFun._2 * 100, (derefCallBase._4 * 1.0d) / derefCallBase._2 * 100,
        
        derefNewCons._2, derefNewBase._2, derefInsObj._2, derefInsCons._2, derefIn._2, derefThis._2,
        derefNewConsAvg, derefNewBaseAvg, derefInsObjAvg, derefInsConsAvg, derefInAvg, derefThisAvg,
        derefNewCons._3, derefNewBase._3, derefInsObj._3, derefInsCons._3, derefIn._3, derefThis._3,
        
        (derefNewCons._4 * 1.0d) / derefNewCons._2 * 100, (derefNewBase._4 * 1.0d) / derefNewBase._2 * 100,
        (derefInsObj._4 * 1.0d)  / derefInsObj._2  * 100, (derefInsCons._4 * 1.0d) / derefInsCons._2 * 100,
        (derefIn._4 * 1.0d)      / derefIn._2      * 100, (derefThis._4 * 1.0d)    / derefThis._2    * 100)
    
    /* non-function */
    val nfunTable = 
"""=============== Definite-Function ===============
| definite call ratio(%%)            : %8.2f  |
| definite function call ratio(%%)   : %8.2f  |
| definite constructor call raito(%%): %8.2f  |
=================================================
""".format(((definiteFun + definiteCons)* 1.0d)/ (callPoint + newPoint) * 100,
          (definiteFun * 1.0d)/ callPoint * 100, (definiteCons * 1.0d)/ newPoint * 100)
      
    /* type */
    val typAvg = 
      if (typ._3 > 0)  (typ._1 * 1.0d)/ typ._3
      else  0.0
    val typBoolAvg = 
      if (typBool._3 > 0)  (typBool._1 * 1.0d)/ typBool._3
      else  0.0
    val typNumAvg = 
      if (typNum._3 > 0)  (typNum._1 * 1.0d)/ typNum._3
      else  0.0
    val typStrAvg = 
      if (typStr._3 > 0)  (typStr._1 * 1.0d)/ typStr._3
      else  0.0
    val typObjAvg = 
      if (typObj._3 > 0)  (typObj._1 * 1.0d)/ typObj._3
      else  0.0
      
    val typeTable =
"""========================= Type ==========================
| type conversion point(#): %8d                    |
| average type size(#)    : %8.2f                    |
| maximum type size(#)    : %8d                    |
| definite type ratio(%%)  : %8.2f                    |
| has undefinted ratio(%%) : %8.2f                    |
|-----------+--toBool--+---toNum--+---toStr--+---toObj--|
|point   (#)| %8d | %8d | %8d | %8d |
|average (#)| %8.2f | %8.2f | %8.2f | %8.2f |
|maximum (#)| %8d | %8d | %8d | %8d |
|definite(%%)| %8.2f | %8.2f | %8.2f | %8.2f |
|hasundef(%%)| %8.2f | %8.2f | %8.2f | %8.2f |
=========================================================
""".format(
        typ._3, typAvg, typ._4, (typ._5 * 1.0d) / typ._3 * 100, (typ._2 * 1.0d) / typ._3 * 100,
        typBool._3, typNum._3, typStr._3, typObj._3,
        typBoolAvg, typNumAvg, typStrAvg, typObjAvg,
        typBool._4, typNum._4, typStr._4, typObj._4,
        
        (typBool._5 * 1.0d) / typBool._3 * 100, (typNum._5 * 1.0d) / typNum._3 * 100,
        (typStr._5 * 1.0d)  / typStr._3  * 100, (typObj._5 * 1.0d) / typObj._3 * 100,
        
        (typBool._2 * 1.0d) / typBool._3 * 100, (typNum._2 * 1.0d) / typNum._3 * 100,
        (typStr._2 * 1.0d)  / typStr._3  * 100, (typObj._2 * 1.0d) / typObj._3 * 100)
    

    val acsTable =
"""============== Property Access ===============
| property access point(#): %8d         |
| constant access ratio(%%): %8.2f         |
|-----------+---Load---+---Store--+--Delete--|
|point   (#)| %8d | %8d | %8d |
|constant(%%)| %8.2f | %8.2f | %8.2f |
==============================================
""".format(
        access._2, (access._1 * 1.0d)/ access._2 * 100,
        accessLoad._2, accessStore._2, accessDel._2,
        (accessLoad._1 * 1.0d)/ accessLoad._2 * 100,
        (accessStore._1 * 1.0d)/ accessStore._2 * 100,
        (accessDel._1 * 1.0d)/ accessDel._2 * 100)
    
    val updateTable = 
"""============ Strong update ============
| update point(#)       : %8d    |
| strong update ratio(%%): %8.2f    |
=======================================
""".format(updatePoint, (updateStrong * 1.0d)/ updatePoint * 100)
    
    val deadTable = 
"""============ Dead instructions ============
| active basic block(#)      : %8d   |
| dead instructions(#)       : %8d   |
| covered basic block(#)     : %8d   |
===========================================
""".format(totalBB - deadBB, deadInst, coveredBBSize)

    val exTable =
"""============ Exception ============
| Total exception(#) : %8d   |
| Error(#)           : %8d   |
| Eval Error(#)      : %8d   |
| Range Error(#)     : %8d   |
| Reference Error(#) : %8d   |
| Syntax Error(#)    : %8d   |
| Type Error(#)      : %8d   |
| URI Error(#)       : %8d   |
===================================
""".format(totalEx, errEx, evalEx, rangeEx, refEx, syntaxEx, typeEx, uriEx)



  // _1._1 => callable sites point (node)
  // _1._2 => total callee function (node)
  // _1._3 => max callee function (node)
  // _1._4 => definite callsite (node)
  // _2._1 => callable sites point (cp)
  // _2._2 => total callee function (cp)
  // _2._3 => max callee function (cp)
  // _2._4 => definite callsite (cp)
//  var callable: CallableStat = ((0,0,0,0),(0,0,0,0))
//  var callableCFGCall: CallableStat = ((0,0,0,0),(0,0,0,0))
//  var callableCFGConstruct: CallableStat = ((0,0,0,0),(0,0,0,0))
//  var callableAPIApply: CallableStat = ((0,0,0,0),(0,0,0,0))

    val callableCalleeNodeAvg = 
      if (callable._1._1 > 0)  (callable._1._2 * 1.0d) / callable._1._1
      else  0.0
    val callableCalleeCPAvg = 
      if (callable._2._1 > 0)  (callable._2._2 * 1.0d) / callable._2._1
      else  0.0
    val callableDefiniteNodeAvg = 
      if (callable._1._1 > 0)  (callable._1._4 * 1.0d) / callable._1._1 * 100.0
      else  0.0
    val callableDefiniteCPAvg = 
      if (callable._2._1 > 0)  (callable._2._4 * 1.0d) / callable._2._1 * 100.0
      else  0.0

    val callableCFGCallCalleeNodeAvg = 
      if (callableCFGCall._1._1 > 0)  (callableCFGCall._1._2 * 1.0d) / callableCFGCall._1._1
      else  0.0
    val callableCFGCallCalleeCPAvg = 
      if (callableCFGCall._2._1 > 0)  (callableCFGCall._2._2 * 1.0d) / callableCFGCall._2._1
      else  0.0
    val callableCFGCallDefiniteNodeAvg = 
      if (callableCFGCall._1._1 > 0)  (callableCFGCall._1._4 * 1.0d) / callableCFGCall._1._1 * 100.0
      else  0.0
    val callableCFGCallDefiniteCPAvg = 
      if (callableCFGCall._2._1 > 0)  (callableCFGCall._2._4 * 1.0d) / callableCFGCall._2._1 * 100.0
      else  0.0

    val callableCFGConstructCalleeNodeAvg = 
      if (callableCFGConstruct._1._1 > 0)  (callableCFGConstruct._1._2 * 1.0d) / callableCFGConstruct._1._1
      else  0.0
    val callableCFGConstructCalleeCPAvg = 
      if (callableCFGConstruct._2._1 > 0)  (callableCFGConstruct._2._2 * 1.0d) / callableCFGConstruct._2._1
      else  0.0
    val callableCFGConstructDefiniteNodeAvg = 
      if (callableCFGConstruct._1._1 > 0)  (callableCFGConstruct._1._4 * 1.0d) / callableCFGConstruct._1._1 * 100.0
      else  0.0
    val callableCFGConstructDefiniteCPAvg = 
      if (callableCFGConstruct._2._1 > 0)  (callableCFGConstruct._2._4 * 1.0d) / callableCFGConstruct._2._1 * 100.0
      else  0.0

    val callableAPIApplyCalleeNodeAvg = 
      if (callableAPIApply._1._1 > 0)  (callableAPIApply._1._2 * 1.0d) / callableAPIApply._1._1
      else  0.0
    val callableAPIApplyCalleeCPAvg = 
      if (callableAPIApply._2._1 > 0)  (callableAPIApply._2._2 * 1.0d) / callableAPIApply._2._1
      else  0.0
    val callableAPIApplyDefiniteNodeAvg = 
      if (callableAPIApply._1._1 > 0)  (callableAPIApply._1._4 * 1.0d) / callableAPIApply._1._1 * 100.0
      else  0.0
    val callableAPIApplyDefiniteCPAvg = 
      if (callableAPIApply._2._1 > 0)  (callableAPIApply._2._4 * 1.0d) / callableAPIApply._2._1 * 100.0
      else  0.0
      
    val callableAPICallCalleeNodeAvg = 
      if (callableAPICall._1._1 > 0)  (callableAPICall._1._2 * 1.0d) / callableAPICall._1._1
      else  0.0
    val callableAPICallCalleeCPAvg = 
      if (callableAPICall._2._1 > 0)  (callableAPICall._2._2 * 1.0d) / callableAPICall._2._1
      else  0.0
    val callableAPICallDefiniteNodeAvg = 
      if (callableAPICall._1._1 > 0)  (callableAPICall._1._4 * 1.0d) / callableAPICall._1._1 * 100.0
      else  0.0
    val callableAPICallDefiniteCPAvg = 
      if (callableAPICall._2._1 > 0)  (callableAPICall._2._4 * 1.0d) / callableAPICall._2._1 * 100.0
      else  0.0

      val callNodeTable =  
"""===================== Function Call for Node ======================
| callablesite point(#)       : %8d                          |
| average callee function(#)  : %8.2f                          |
| maximum callee function(#)  : %8d                          |
| definite call ratio(%%)      : %8.2f                          |
|-----------+--CFGCall--+--CFGConstruct--+--APIApply--+--APICall--|
|point   (#)| %9d | %14d | %10d | %9d |
|average (#)| %9.2f | %14.2f | %10.2f | %9.2f |
|maximum (#)| %9d | %14d | %10d | %9d |
|definite(%%)| %9.2f | %14.2f | %10.2f | %9.2f |
|totalnum(#)| %9d | %14d | %10d | %9d |
===================================================================
""".format(
        callable._1._1, callableCalleeNodeAvg, callable._1._3, callableDefiniteNodeAvg,
        callableCFGCall._1._1, callableCFGConstruct._1._1, callableAPIApply._1._1, callableAPICall._1._1,
        callableCFGCallCalleeNodeAvg, callableCFGConstructCalleeNodeAvg, callableAPIApplyCalleeNodeAvg, callableAPICallCalleeNodeAvg,
        callableCFGCall._1._3, callableCFGConstruct._1._3, callableAPIApply._1._3, callableAPICall._1._3,
        callableCFGCallDefiniteNodeAvg, callableCFGConstructDefiniteNodeAvg, callableAPIApplyDefiniteNodeAvg, callableAPICallDefiniteNodeAvg,
        callableCFGCall._1._2, callableCFGConstruct._1._2, callableAPIApply._1._2, callableAPICall._1._2)

      val callCPTable =  
"""================= Function Call for ControlPoint ==================
| callablesite point(#)       : %8d                          |
| average callee function(#)  : %8.2f                          |
| maximum callee function(#)  : %8d                          |
| definite call ratio(%%)      : %8.2f                          |
|-----------+--CFGCall--+--CFGConstruct--+--APIApply--+--APICall--|
|point   (#)| %9d | %14d | %10d | %9d |
|average (#)| %9.2f | %14.2f | %10.2f | %9.2f |
|maximum (#)| %9d | %14d | %10d | %9d |
|definite(%%)| %9.2f | %14.2f | %10.2f | %9.2f |
|totalnum(#)| %9d | %14d | %10d | %9d |
===================================================================
""".format(
        callable._2._1, callableCalleeCPAvg, callable._2._3, callableDefiniteCPAvg,
        callableCFGCall._2._1, callableCFGConstruct._2._1, callableAPIApply._2._1, callableAPICall._2._1,
        callableCFGCallCalleeCPAvg, callableCFGConstructCalleeCPAvg, callableAPIApplyCalleeCPAvg, callableAPICallCalleeCPAvg,
        callableCFGCall._2._3, callableCFGConstruct._2._3, callableAPIApply._2._3, callableAPICall._2._3,
        callableCFGCallDefiniteCPAvg, callableCFGConstructDefiniteCPAvg, callableAPIApplyDefiniteCPAvg, callableAPICallDefiniteCPAvg,
        callableCFGCall._2._2, callableCFGConstruct._2._2, callableAPIApply._2._2, callableAPICall._2._2)
        
      val calleeSizeNodeTable =  
"""================ Function Callee size for Node ================
| 0-callee point(#)       : %8d                          |
| 1-callee point(#)       : %8d                          |
| 2-callees point(#)      : %8d                          |
| 3-callees point(#)      : %8d                          |
| > 3-callees point(#)    : %8d                          |
===============================================================
""".format(
        callableCalleeSize._1._1, callableCalleeSize._1._2, callableCalleeSize._1._3, callableCalleeSize._1._4, callableCalleeSize._1._5)
      val calleeSizeCPTable =  
"""============ Function Callee size for ControlPoint ============
| 0-callee point(#)       : %8d                          |
| 1-callee point(#)       : %8d                          |
| 2-callees point(#)      : %8d                          |
| 3-callees point(#)      : %8d                          |
| > 3-callees point(#)    : %8d                          |
===============================================================
""".format(
        callableCalleeSize._2._1, callableCalleeSize._2._2, callableCalleeSize._2._3, callableCalleeSize._2._4, callableCalleeSize._2._5)
        
      val derefSizeNodeTable =  
"""===================== Deref size for Node =====================
| 0-object point(#)       : %8d                          |
| 1-object point(#)       : %8d                          |
| 2-objects point(#)       : %8d                         |
| 3-objects point(#)       : %8d                         |
| 4-objects point(#)       : %8d                         |
| 5-objects point(#)       : %8d                         |
| > 5-objects point(#)     : %8d                         |
===============================================================
""".format(
        derefSizeNode(0), derefSizeNode(1), derefSizeNode(2), derefSizeNode(3), derefSizeNode(4), derefSizeNode(5), derefSizeNode(6))
      val derefSizeCPTable =  
"""================= Deref size for ControlPoint =================
| 0-object point(#)       : %8d                          |
| 1-object point(#)       : %8d                          |
| 2-objects point(#)       : %8d                         |
| 3-objects point(#)       : %8d                         |
| 4-objects point(#)       : %8d                         |
| 5-objects point(#)       : %8d                         |
| > 5-objects point(#)     : %8d                         |
===============================================================
""".format(
        derefSizeCP(0), derefSizeCP(1), derefSizeCP(2), derefSizeCP(3), derefSizeCP(4), derefSizeCP(5), derefSizeCP(6))
        
    //print(derefTable + nfunTable + typeTable + acsTable + updateTable + deadTable + exTable + callNodeTable + callCPTable
    //    + calleeSizeNodeTable + calleeSizeCPTable + derefSizeNodeTable + derefSizeCPTable)
    /*  */

    //val filename = getFuncInfo(fid).getSpan().begin.getFileName()
    //if(filename.endsWith("__builtin__.js") || filename.endsWith("__dom__.js") || filename.endsWith("__input__.js")) { false }
    val userfidSet = (cfg.getFunctionIds - 0).filter(fid => {
        val filename = cfg.getFuncInfo(fid).getSpan().begin.getFileName()
        !(builtins.contains(fid)|| (filename.endsWith("__builtin__.js") || filename.endsWith("__dom__.js") || filename.endsWith("__input__.js")))
      })

    val usedfidSet = userfidSet.filter(fid => {
       inTable.get((fid, LEntry)) match {
         case Some(cs) => cs.exists(_cs => _cs._2 </ StateBot)
         case None => false
       }
    })
    val usedFunSpan = usedfidSet.foldLeft[Set[(FunctionId, Span)]](Set())((s,fid) => s + ((fid, cfg.getFuncInfo(fid).getSpan)))
    val totalCallSet = cfg.getCallFromAftercallMap.foldLeft[Set[Span]](Set())((cs, m) => {
      val callNode = m._2
      val fid = callNode._1
      val last_inst = cfg.getLastInst(callNode)
      val filename = last_inst.getInfo match {
        case Some(in) => in.getSpan().begin.getFileName()
        case None => ""
      }  
      if(!(builtins.contains(fid)|| (filename.endsWith("dOMEvent") || filename.endsWith("__builtin__.js") || filename.endsWith("__dom__.js") || filename.endsWith("__input__.js")))){
        last_inst.getInfo match {
          case Some(in) => 
            //println("span info : " + in.getSpan)
            cs + in.getSpan
          case None => cs
        }
      }
      else
        cs
    })
/* 
    val sortedFun = usedFunSpan.toList.sortWith((n1, n2) => {
          n1._2.begin.getLine <= n2._2.begin.getLine
    })
  */
    val sortedFun = usedFunSpan.toList.sortWith((n1, n2) => {
          n1._1 <= n2._1
    })
/*
    println("**** Reachable Functions ****")
    sortedFun.foreach(n=> {
       println("reachable function : " + n);
    })
*/

/*
    println("\n**** Non-Function Calls ****") 
    val sortedImpreCall = impreCallSet.toList.sortWith((n1, n2) => {
          n1._1._1 <= n2._1._1
    })
    sortedImpreCall.foreach(n=> {
      println("imprecise call : " + n);
   })
    println("\n**** Absent Variables ****")
   val sortedAbsentVar = absentVarSet.toList.sortWith((n1, n2) => {
          n1._1._1 < n2._1._1
    })
    sortedAbsentVar.foreach(n=> {
      println("absent var : " + n);
   })
   println("\n**** Illegal Property Access ****")
   val sortedNullProp = nullUndefPropertyAccessSet.toList.sortWith((n1, n2) => {
          n1._1._1 <= n2._1._1
    })
    sortedNullProp.foreach(n=> {
      println("property access : " + n);
   })

*/
    /*
    
    val sortedCall = totalCallSet.toList.sortWith((n1, n2) => {
          n1.begin.getLine <= n2.begin.getLine
    })
    
    val sortedProp = NodeUtil.sourceStatistics.propertyAccessSet.toList.sortWith((n1, n2) => {
          n1.begin.getLine <= n2.begin.getLine
    })
    
   
     sortedProp.foreach(n=> {
       println("prop span : " + n);
     })

    var tajsSourceMap: Map[Int, Int] = HashMap() 
    val filename = "out.txt"
    for(line <-Source.fromFile(filename).getLines()){
       val linenum = line.mkString.toInt
       tajsSourceMap.get(linenum) match {
         case Some(count) => 
           tajsSourceMap = tajsSourceMap + (linenum -> (tajsSourceMap(linenum) + 1))
         case None => tajsSourceMap = tajsSourceMap + (linenum -> 1)
       }
    }
    
    var safeSourceMap: Map[Int, Int] = HashMap() 
    NodeUtil.sourceStatistics.propertyAccessSet.foreach(s => {
       val linenum = s.begin.getLine
       safeSourceMap.get(linenum) match {
         case Some(count) => 
           safeSourceMap = safeSourceMap + (linenum -> (safeSourceMap(linenum) + 1))
         case None => safeSourceMap = safeSourceMap + (linenum -> 1)
       }
    })
    println("TAJS source ")
    tajsSourceMap.foreach(e => safeSourceMap.get(e._1) match {
      case Some(count) if count == e._2 => ()
      case _ => println("TAJS line number : " + e._1)
    })
    println("SAFE source ")
    safeSourceMap.foreach(e => tajsSourceMap.get(e._1) match {
      case Some(count) if count == e._2 => ()
      case _ => println("SAFE line number : " + e._1)
    })
   */
   /*
   val absentVar = absentVarSet.toList.sortWith((n1, n2) => {
          n1.begin.getLine <= n2.begin.getLine
    })
    absentVar.foreach(n=> {
      println("var span : " + n);
   })
*/

   /*
    val sortedVar = NodeUtil.sourceStatistics.variableReadSet.toList.sortWith((n1, n2) => {
          n1.begin.getLine <= n2.begin.getLine
    })
    sortedVar.foreach(n=> {
      println("var span : " + n);
   })

    var tajsSourceMap: Map[Int, Int] = HashMap() 
    val filename = "out2.txt"
    for(line <-Source.fromFile(filename).getLines()){
       val linenum = line.mkString.toInt
       tajsSourceMap.get(linenum) match {
         case Some(count) => 
           tajsSourceMap = tajsSourceMap + (linenum -> (tajsSourceMap(linenum) + 1))
         case None => tajsSourceMap = tajsSourceMap + (linenum -> 1)
       }
    }
 
    var safeSourceMap: Map[Int, Int] = HashMap() 
    NodeUtil.sourceStatistics.variableReadSet.foreach(s => {
       val linenum = s.begin.getLine
       safeSourceMap.get(linenum) match {
         case Some(count) => 
           safeSourceMap = safeSourceMap + (linenum -> (safeSourceMap(linenum) + 1))
         case None => safeSourceMap = safeSourceMap + (linenum -> 1)
       }
    })
    println("TAJS source ")
    tajsSourceMap.foreach(e => safeSourceMap.get(e._1) match {
      case Some(count) if count == e._2 => ()
      case _ => println("TAJS line number : " + e._1)
    })
    println("SAFE source ")
    safeSourceMap.foreach(e => tajsSourceMap.get(e._1) match {
      case Some(count) if count == e._2 => ()
      case _ => println("SAFE line number : " + e._1)
    })
    */
    //sortedCall.foreach(n=> {
      //println("span : " + n);
    //})

    val nonZeroMcPerInst = mcPerInst.values.count(callsPerState => callsPerState.exists(_ > 1))
    val nonZeroMdPerExpr = mdPerExpr.values.count(derefsPerState => derefsPerState.exists(_ > 1))
    
    val newTable1 = 
"""=============== Statistics =================================
| Multiple dereference points (MD) (#)     : %d
| Multiple call points (MC) (#) : %d
| Multiple property access points (PR) (#) : %d                              
=============================================================== 
""".format(/*deref._2 - deref._4,*/ nonZeroMdPerExpr,
  /*callable._2._1 - callable._2._4,*/ nonZeroMcPerInst,
  access._2 - access._1)


    print(newTable1)



  }
  def printDump() = {
    /* dereference */
    val derefAvg = 
      if (deref._2 > 0)  (deref._1 * 1.0d)/ deref._2
      else  0.0
    val derefLoadAvg = 
      if (derefLoad._2 > 0)  (derefLoad._1 * 1.0d)/ derefLoad._2
      else  0.0
    val derefStoreAvg = 
      if (derefStore._2 > 0)  (derefStore._1 * 1.0d)/ derefStore._2
      else  0.0
    val derefDelAvg = 
      if (derefDel._2 > 0)  (derefDel._1 * 1.0d)/ derefDel._2
      else  0.0
    val derefCallFunAvg = 
      if (derefCallFun._2 > 0)  (derefCallFun._1 * 1.0d)/ derefCallFun._2
      else  0.0
    val derefCallBaseAvg = 
      if (derefCallBase._2 > 0)  (derefCallBase._1 * 1.0d)/ derefCallBase._2
      else  0.0
    val derefNewConsAvg = 
      if (derefNewCons._2 > 0)  (derefNewCons._1 * 1.0d)/ derefNewCons._2
      else  0.0
    val derefNewBaseAvg = 
      if (derefNewBase._2 > 0)  (derefNewBase._1 * 1.0d)/ derefNewBase._2
      else  0.0
    val derefAllocAvg = 
      if (derefAlloc._2 > 0)  (derefAlloc._1 * 1.0d)/ derefAlloc._2
      else  0.0
    val derefInsObjAvg = 
      if (derefInsObj._2 > 0)  (derefInsObj._1 * 1.0d)/ derefInsObj._2
      else  0.0
    val derefInsConsAvg = 
      if (derefInsCons._2 > 0)  (derefInsCons._1 * 1.0d)/ derefInsCons._2
      else  0.0
    val derefInAvg = 
      if (derefIn._2 > 0)  (derefIn._1 * 1.0d)/ derefIn._2
      else  0.0
    val derefThisAvg = 
      if (derefThis._2 > 0)  (derefThis._1 * 1.0d)/ derefThis._2
      else  0.0

      
      
    val derefDump = 
"""============ Dereference ============
# dereference point(#): %d
# average dereference(#): %f
# maximum dereference(#): %d
# definite deref. ratio(%%): %f
-- Load --
# lo dereference point(#)    : %d
# lo average dereference(#)  : %f
# lo maximum dereference(#)  : %d
# lo definite deref. ratio(%%): %f
-- Store --
# st dereference point(#)    : %d
# st average dereference(#)  : %f
# st maximum dereference(#)  : %d
# st definite deref. ratio(%%): %f
-- DeleteProp --
# de dereference point(#)    : %d
# de average dereference(#)  : %f
# de maximum dereference(#)  : %d
# de definite deref. ratio(%%): %f
-- Call(function) --
# cf dereference point(#)    : %d
# cf average dereference(#)  : %f
# cf maximum dereference(#)  : %d
# cf definite deref. ratio(%%): %f
-- Call(base) --
# cb dereference point(#)    : %d
# cb average dereference(#)  : %f
# cb maximum dereference(#)  : %d
# cb definite deref. ratio(%%): %f
-- New(constructor) --
# nc dereference point(#)    : %d
# nc average dereference(#)  : %f
# nc maximum dereference(#)  : %d
# nc definite deref. ratio(%%): %f
-- New(base) --
# nb dereference point(#)    : %d
# nb average dereference(#)  : %f
# nb maximum dereference(#)  : %d
# nb definite deref. ratio(%%): %f
-- Alloc --
# al dereference point(#)    : %d
# al average dereference(#)  : %f
# al maximum dereference(#)  : %d
# al definite deref. ratio(%%): %f
 -- instanceof(object) --
# io dereference point(#)    : %d
# io average dereference(#)  : %f
# io maximum dereference(#)  : %d
# io definite deref. ratio(%%): %f
 -- instanceof(constructor) --
# ic dereference point(#)    : %d
# ic average dereference(#)  : %f
# ic maximum dereference(#)  : %d
# ic definite deref. ratio(%%): %f
 -- in --
# in dereference point(#)    : %d
# in average dereference(#)  : %f
# in maximum dereference(#)  : %d
# in definite deref. ratio(%%): %f
 -- this --
# th dereference point(#)    : %d
# th average dereference(#)  : %f
# th maximum dereference(#)  : %d
# th definite deref. ratio(%%): %f
""".format(
      deref._2,         derefAvg,         deref._3,         (deref._4 * 1.0d)         / deref._2         * 100,
      derefLoad._2,     derefLoadAvg,     derefLoad._3,     (derefLoad._4 * 1.0d)     / derefLoad._2     * 100,
      derefStore._2,    derefStoreAvg,    derefStore._3,    (derefStore._4 * 1.0d)    / derefStore._2    * 100,
      derefDel._2,      derefDelAvg,      derefDel._3,      (derefDel._4 * 1.0d)      / derefDel._2      * 100,
      derefCallFun._2,  derefCallFunAvg,  derefCallFun._3,  (derefCallFun._4 * 1.0d)  / derefCallFun._2  * 100,
      derefCallBase._2, derefCallBaseAvg, derefCallBase._3, (derefCallBase._4 * 1.0d) / derefCallBase._2 * 100,
      derefNewCons._2,  derefNewConsAvg,  derefNewCons._3,  (derefNewCons._4 * 1.0d)  / derefNewCons._2  * 100,
      derefNewBase._2,  derefNewBaseAvg,  derefNewBase._3,  (derefNewBase._4 * 1.0d)  / derefNewBase._2  * 100,
      derefAlloc._2,    derefAllocAvg,    derefAlloc._3,    (derefAlloc._4 * 1.0d)    / derefAlloc._2    * 100,
      derefInsObj._2,   derefInsObjAvg,   derefInsObj._3,   (derefInsObj._4 * 1.0d)   / derefInsObj._2   * 100,
      derefInsCons._2,  derefInsConsAvg,  derefInsCons._3,  (derefInsCons._4 * 1.0d)  / derefInsCons._2  * 100,
      derefIn._2,       derefInAvg,       derefIn._3,       (derefIn._4 * 1.0d)       / derefIn._2       * 100,
      derefThis._2,     derefThisAvg,     derefThis._3,     (derefThis._4 * 1.0d)     / derefThis._2     * 100)
    
    val nfunDump = 
"""
=============== Definite-Function ===============
# definite call ratio(%%): %f      
# definite function call ratio(%%): %f
# definite constructor call raito(%%): %f
""".format(((definiteFun + definiteCons)* 1.0d)/ (callPoint + newPoint) * 100,
    (definiteFun * 1.0d)/ callPoint * 100, (definiteCons * 1.0d)/ newPoint * 100)
      
    /* type */
    val typAvg = 
      if (typ._3 > 0)  (typ._1 * 1.0d)/ typ._3
      else  0.0
    val typBoolAvg = 
      if (typBool._3 > 0)  (typBool._1 * 1.0d)/ typBool._3
      else  0.0
    val typNumAvg = 
      if (typNum._3 > 0)  (typNum._1 * 1.0d)/ typNum._3
      else  0.0
    val typStrAvg = 
      if (typStr._3 > 0)  (typStr._1 * 1.0d)/ typStr._3
      else  0.0
    val typObjAvg = 
      if (typObj._3 > 0)  (typObj._1 * 1.0d)/ typObj._3
      else  0.0
    val typeDump = 
"""============ Type ============
# type conversion point(#): %d
# average type size(#): %f
# maximum type size(#): %d
# definite type ratio(%%): %f
# has undefined ratio(%%): %f
-- toBoolean --
# type conversion point(#) : %d
# average type size(#) : %f
# maximum type size(#) : %d
# definite type ratio(%%) : %f
# has undefinted ratio(%%) : %f
-- toNumber --
# type conversion point(#) : %d
# average type size(#) : %f
# maximum type size(#) : %d
# definite type ratio(%%) : %f
# has undefinted ratio(%%) : %f
-- toString --
# type conversion point(#) : %d
# average type size(#) : %f
# maximum type size(#) : %d
# definite type ratio(%%) : %f
# has undefinted ratio(%%) : %f
-- toObject --
# type conversion point(#) : %d
# average type size(#) : %f
# maximum type size(#) : %d
# definite type ratio(%%) : %f
# has undefinted ratio(%%) : %f
""".format(
      typ._3, typAvg, typ._4, (typ._5 * 1.0d) / typ._3 * 100, (typ._2 * 1.0d)/ typ._3 * 100,
      typBool._3, typBoolAvg, typBool._4, (typBool._5 * 1.0d) / typBool._3 * 100, (typBool._2 * 1.0d)/ typBool._3 * 100,
      typNum._3, typNumAvg, typNum._4, (typNum._5 * 1.0d) / typNum._3 * 100, (typNum._2 * 1.0d)/ typNum._3 * 100,
      typStr._3, typStrAvg, typStr._4, (typStr._5 * 1.0d) / typStr._3 * 100, (typStr._2 * 1.0d)/ typStr._3 * 100,
      typObj._3, typObjAvg, typObj._4, (typObj._5 * 1.0d) / typObj._3 * 100, (typObj._2 * 1.0d)/ typObj._3 * 100)

      
    /* property access */
    val acsDump =
"""============ Property Access ============
# property access point(#): %d
# constant access ratio(%%): %f
-- Load --
# lo property access point(#): %d 
# lo constant access ratio(%%): %f
-- Store --
# st property access point(#): %d
# st constant access ratio(%%): %f
-- Delete Prop --
# de property access point(#): %d
# de constant access ratio(%%): %f
""".format(
        access._2,      (access._1 * 1.0d)/ access._2 * 100,
        accessLoad._2,  (accessLoad._1 * 1.0d)/ accessLoad._2 * 100,
        accessStore._2, (accessStore._1 * 1.0d)/ accessStore._2 * 100,
        accessDel._2,   (accessDel._1 * 1.0d)/ accessDel._2 * 100)
    
    /* strong update */
    val updateDump =
"""============ Strong update ============
# update point(#): %d
# strong update ratio(%%): %f
""".format(updatePoint, (updateStrong * 1.0d)/ updatePoint * 100)
    
    /* dead inst. */
    val deadDump =
"""============ Dead instructions ============
# active basic block(#): %d
# dead instructions(#): %d
""".format(totalBB - deadBB, deadInst)
    
    val exDump =
"""============ Exception ============
# Total exception(#): %d
# Error(#): %d
# Eval Error(#): %d
# Range Error(#): %d
# Reference Error(#): %d
# Syntax Error(#): %d
# Type Error(#): %d
# URI Error(#): %d
""".format(totalEx, errEx, evalEx, rangeEx, refEx, syntaxEx, typeEx, uriEx)
    print(derefDump + nfunDump + typeDump + acsDump + updateDump + deadDump + exDump)
  }
  
  def countCall(cstate: CState, inst: CFGInst, what: String): Unit = {
    def getCallableFunctionSet(inst: CFGInst, fidset: FunSet): FunSet = {
      if(preCallGraph == null) fidset
      else {
        preCallGraph.get(inst) match {
          case Some(funset) => fidset.intersect(funset)
          case None => FunSetBot
        }
      }
    }
    def incr(size: Int, info: (Int, Int, Int, Int, Int)): (Int, Int, Int, Int, Int) = {
      size match {
        case 0 => (info._1 + 1, info._2, info._3, info._4, info._5)
        case 1 => (info._1, info._2 + 1, info._3, info._4, info._5)
        case 2 => (info._1, info._2, info._3 + 1, info._4, info._5)
        case 3 => (info._1, info._2, info._3, info._4 + 1, info._5)
        case _ => (info._1, info._2, info._3, info._4, info._5 + 1)
      }
    }

    
  // _1._1 => callable sites point (node)
  // _1._2 => total callee function (node)
  // _1._3 => max callee function (node)
  // _1._4 => definite callsite (node)
  // _2._1 => callable sites point (cp)
  // _2._2 => total callee function (cp)
  // _2._3 => max callee function (cp)
  // _2._4 => definite callsite (cp)
    val (call_, fun_expr) = what match {
      case "CFGCall"       => (callableCFGCall, inst.asInstanceOf[CFGCall].fun)
      case "CFGConstruct"  => (callableCFGConstruct, inst.asInstanceOf[CFGConstruct].cons)
      case "APIApply"      => (callableAPIApply, CFGThis(null))
      case "APICall"       => (callableAPICall, CFGThis(null))
    }

    // (callable info, total calleeset)
    var hasHeap = false
    val (calleeset, call_cp) = cstate.foldLeft((FunSetBot, call_._2))((curr, kv) => {
      if (kv._2._1 <= HeapBot) curr
      else {
        hasHeap = true
        val fun_val = SE.V(fun_expr, kv._2._1, kv._2._2)._1
          //inst.getInfo match {
          //  case Some(in) =>
          //    println("call span : " + in.getSpan)
          //    println("fun_val : " + fun_val)
              //impreciseCallSet = impreciseCallSet + in.getSpan
          //  case None =>()
          //}

        //LSA
       /* 
        if(fun_val._1 </ PValueBot || !(fun_val._2.size !=0 && fun_val._2.forall(l=> BoolTrue == Helper.IsCallable(kv._2._1, l))))
          inst.getInfo match {
            case Some(in) =>
              val node = cfg.findEnclosingNode(inst)
              impreciseCallSet = impreciseCallSet + ((node, in.getSpan))
            case None =>()
          }
        */
        val fun_locset = fun_val._2.filter(l => BoolTrue <= Helper.IsCallable(kv._2._1,l))   
        val funset_ = fun_locset.foldLeft(FunSetBot)((fset, l) => {
          val calleeset =
            if(what.equals("CFGConstruct")) kv._2._1(l)("@construct")._3
            else kv._2._1(l)("@function")._3
          fset ++ calleeset
        })
        val funset = getCallableFunctionSet(inst, funset_)
        val callee_size = funset.size
        // _2._1 => callable sites point (cp)
        // _2._2 => total callee function (cp)
        // _2._3 => max callee function (cp)
        // _2._4 => definite callsite (cp)
        val call = (curr._2._1 + 1, curr._2._2 + callee_size, chooseMax(curr._2._3, callee_size), if(callee_size == 1) curr._2._4 + 1 else curr._2._4)
        // update callable for call-context
        if(callee_size == 0) {
          //println("\t * WARNING : No function call is possible in CP ("+sem.getCFG.findEnclosingNode(inst)+")" + inst)
          // ignore dummy function callsite
        }
        else {
          callableCalleeSize = (callableCalleeSize._1, incr(callee_size, callableCalleeSize._2))
          callable = (callable._1, (callable._2._1 + 1, callable._2._2 + callee_size, chooseMax(callable._2._3, callee_size), if(callee_size == 1) callable._2._4 + 1 else callable._2._4))

          mcPerInst = mcPerInst + (inst -> (mcPerInst.getOrElse(inst, List()) ++ List(callee_size)))
        }
        (curr._1 ++ funset, call)
      }
    })
    if(hasHeap) {
      // _1._1 => callable sites point (node)
      // _1._2 => total callee function (node)
      // _1._3 => max callee function (node)
      // _1._4 => definite callsite (node)
      if(calleeset.isEmpty) {
      //  println(" * WARNING : No function call is possible in ("+sem.getCFG.findEnclosingNode(inst)+")" + inst + " : " + calleeset.size)
      }
      else {
        callableCalleeSize = (incr(calleeset.size, callableCalleeSize._1), callableCalleeSize._2)
        callable = ((callable._1._1 + 1, callable._1._2 + calleeset.size, chooseMax(callable._1._3, calleeset.size), if(calleeset.size == 1) callable._1._4 + 1 else callable._1._4), callable._2)
        val call_node = (call_._1._1 + 1, call_._1._2 + calleeset.size, chooseMax(call_._1._3, calleeset.size), if(calleeset.size == 1) call_._1._4 + 1 else call_._1._4)
        val newCall = (call_node, call_cp)
        // update callable for call-context
        what match {
          case "CFGCall"       => callableCFGCall = newCall
          case "CFGConstruct"  => callableCFGConstruct = newCall
          case "APIApply"      => callableAPIApply = newCall
          case "APICall"       => callableAPICall = newCall
        }
      }
    }

  }

  def countAbsentVar(node: Node, stateMap: CState, span: Span, id: CFGId): Unit = {
    
    val filename = span.begin.getFileName()
    if(filename.endsWith("dOMEvent") || filename.endsWith("__builtin__.js") || filename.endsWith("__dom__.js") || filename.endsWith("__input__.js")) 
      { return }
    // Check for user variable only
    if (!id.isInstanceOf[CFGUserId]) return
    val idAbsString = AbsString.alpha(id.getText)

    val isBug = stateMap.exists(s => {
      val doesExist: AbsBool = id.getVarKind match {
        case PureLocalVar => BoolTrue
        case CapturedVar => BoolTrue
        case CapturedCatchVar => BoolTrue
        case GlobalVar => Helper.HasProperty(s._2.heap, GlobalLoc, idAbsString)
      }
     /*
     if(doesExist == BoolFalse || doesExist == BoolTop){
        println("id name : " + id.getText)
        println("call context : " + s._1)
        println("Global Heap : " + DomainPrinter.printObj(4, s._2.heap(GlobalLoc)))
      }
*/
      doesExist == BoolFalse || doesExist == BoolTop
    })
    if(isBug && NodeUtil.sourceStatistics.variableReadSet.contains(span)){
        //println("node : " + node)
        //println("var name : " + id)
        absentVarSet = absentVarSet + ((node, span));
    }
  }





  def incrDeref(derefSize: Array[Int], size: Int) = {
    val key = if(size > 5) 6 else size
    derefSize.update(key, derefSize(key) + 1)
  }
  var derefInst: CFGInst = null
  def countDeref(cstate: CState, nextCState: CState, expr: CFGExpr, what: String): Unit = {
    val drf = what match {
      case "Load" =>       derefLoad
      case "Store" =>      derefStore
      case "Delete" =>     derefDel
      case "Call(fun)" =>  derefCallFun
      case "Call(base)" => derefCallBase
      case "New(cons)" =>  derefNewCons
      case "New(base)" =>  derefNewBase
      case "Alloc" =>      derefAlloc
      case "ins(obj)" =>   derefInsObj
      case "ins(cons)" =>  derefInsCons
      case "in" =>         derefIn
      case "this" =>       derefThis
    }
    
    var hasState = false
    val (v_join, count) = cstate.foldLeft((ValueBot,0))((curr, kv) => {
      if (kv._2._1 <= HeapBot || nextCState(kv._1)._1 <= HeapBot) curr 
      else {
        hasState = true
        val v = SE.V(expr, kv._2._1, kv._2._2)._1
        
        // add deref size in CP
        incrDeref(derefSizeCP, locSize(kv._2._1, v._2))

        val count = locSize(kv._2._1, v._2)
        assert(count > 0)
        mdPerExpr = mdPerExpr + (expr -> (mdPerExpr.getOrElse(expr, List()) ++ List(count)))
        
        (v + curr._1, chooseMax(locSize(kv._2._1, v._2), curr._2))
      }
    })
    if(hasState) {
      val mcount = locSize(cstate.foldLeft(StateBot)((s, kv)=> s + kv._2)._1, v_join._2)
    
      // add deref size in CP
      incrDeref(derefSizeNode, mcount)
      //println(" * Deref size in ("+sem.getCFG.findEnclosingNode(derefInst)+")" + derefInst + " ; " + mcount)
    
      /* total deref */
      val maxGlobal = chooseMax(count, deref._3)
      val definiteGlobal = if (count <=  1) deref._4 + 1 else deref._4
      deref = (deref._1 + mcount, deref._2 + 1, maxGlobal, definiteGlobal)
    
      /* selected deref */
      val max = chooseMax(count, drf._3)
      val definite = if (count <= 1) drf._4 + 1 else drf._4
      val new_drf = (drf._1 + mcount, drf._2 + 1, max, definite)

      what match {
        case "Load" =>       derefLoad     = new_drf
        case "Store" =>      derefStore    = new_drf
        case "Delete" =>     derefDel      = new_drf
        case "Call(fun)" =>  derefCallFun  = new_drf
        case "Call(base)" => derefCallBase = new_drf
        case "New(cons)" =>  derefNewCons  = new_drf
        case "New(base)" =>  derefNewBase  = new_drf
        case "Alloc" =>      derefAlloc    = new_drf
        case "ins(obj)" =>   derefInsObj   = new_drf
        case "ins(cons)" =>  derefInsCons  = new_drf
        case "in" =>         derefIn       = new_drf
        case "this" =>       derefThis     = new_drf
    }
    }
  }
  
  def countType(cstate: CState, expr: CFGExpr, what: String): Unit = {
    val ttyp = 
      what match {
        case "Bool" =>   typBool
        case "Number" => typNum
        case "String" => typStr
        case "Object" => typObj
      }

    val (v_join, count) = cstate.foldLeft((ValueBot,0))((curr, kv) => {
      if (kv._2._1 <= HeapBot) curr 
      else {
        val v = SE.V(expr, kv._2._1, kv._2._2)._1
        (v + curr._1, chooseMax(v.typeCount, curr._2))
      }
    })
    val mcount = v_join.typeCount

    /* type total */
    val undefTotal = if (mcount > 1 && UndefTop <= v_join._1._1) typ._2 + 1 else typ._2
    val maxTotal = chooseMax(count, typ._4) 
    val definiteTotal = if (count <= 1) typ._5 + 1 else typ._5
    typ = (typ._1 + mcount, undefTotal, typ._3 + 1, maxTotal, definiteTotal)
    
    /* selected type */
    val undef = if (mcount > 1 && UndefTop <= v_join._1._1) ttyp._2 + 1 else ttyp._2
    val max = chooseMax(count, ttyp._4)
    val definite = if (count <= 1) ttyp._5 + 1 else ttyp._5
    val new_ttyp = (ttyp._1 + mcount, undef, ttyp._3 + 1, max, definite)
    what match {
      case "Bool" =>   typBool = new_ttyp
      case "Number" => typNum  = new_ttyp
      case "String" => typStr  = new_ttyp
      case "Object" => typObj  = new_ttyp
    }
  }
 
  def countAccess(cstate: CState, expr: CFGExpr, what: String): Unit = {
    expr match {
      case CFGString(_) => Unit
      case _ => {
        val acs = what match {
          case "Load" => accessLoad
          case "Store" => accessStore
          case "Delete" => accessDel
        }
        
        val non_single = cstate.exists(kv => {
          val s = kv._2
          if (s._1 <= HeapBot) false 
          else {
            val (v, es) = SE.V(expr, s._1, s._2)
            val str = Helper.toString(Helper.toPrimitive_better(s._1, v))
            str.getAbsCase match {
              case AbsTop | AbsMulti => true 
              case _ => false
            }
          }
        })
        
        /* total access */
        val countTotal = if (non_single) access._1 else access._1 + 1
        val pointTotal = access._2 + 1;
        access = (countTotal, pointTotal)
        
        /* selected access */
        val count = if (non_single) acs._1 else acs._1 + 1 
        val point = acs._2 + 1;
        val new_acs = (count, point)
        what match {
          case "Load" =>   accessLoad  = new_acs
          case "Store" =>  accessStore = new_acs
          case "Delete" => accessDel   = new_acs
        }
      }
    }
  }

  def countVarUpdate(cstate: CState, x: CFGId) = {
    x match {
      case CFGUserId(_,_,_,_,_) => {
        updatePoint = updatePoint + 1
        val weak = cstate.exists(kv => {
          val s = kv._2
          if (s._1 <= HeapBot) false 
          else {
            val lset_base = Helper.LookupBase(s._1, x)
            locSize(s._1, lset_base) match {
              case 0 => false
              case 1 => if (isOldLoc(lset_base.head)) true else false
              case _ => true
            }
          }
        })
        if (!weak) updateStrong = updateStrong + 1
      }
      // case for x is temporal variable.
      case CFGTempId(_,_) => ()
    }
  }

  def countPropUpdate(cstate: CState, obj: CFGExpr) = {
    updatePoint = updatePoint + 1;
    val weak = cstate.exists(kv => {
      val s = kv._2
      if (s._1 <= HeapBot) false 
      else {
        val lset_obj = SE.V(obj, s._1, s._2)._1._2
        locSize(s._1, lset_obj) match {
          case 0 => false
          case 1 => if (isOldLoc(lset_obj.head)) true else false
          case _ => true
        }
      }      
    })
    if (!weak) updateStrong = updateStrong + 1
  }

  def countDefCall(cstate: CState, expr: CFGExpr) = {
    callPoint = callPoint + 1
    val non_definite = cstate.exists(kv => {
      val s = kv._2
      if (s._1 <= HeapBot) false 
      else {
        val lset = SE.V(expr, s._1, s._2)._1._2
        lset.exists(l => BoolFalse <= Helper.IsCallable(s._1, l))
      }      
    })
    if (!non_definite) definiteFun = definiteFun + 1
  }

  def countDefNew(cstate: CState, expr: CFGExpr) = {
    newPoint = newPoint + 1
    val non_definite = cstate.exists(kv => {
      val s = kv._2
      if (s._1 <= HeapBot) false 
      else {
        val lset = SE.V(expr, s._1, s._2)._1._2
        lset.exists(l => BoolFalse <= Helper.HasConstruct(s._1, l))
      }      
    })
    if (!non_definite) definiteCons = definiteCons + 1
  }
  
  def countException(v_ex :Value) = {
    var exist = false
    v_ex._2.foreach((l) => {
      l match {
        case BuiltinError.ErrLoc => errEx = errEx+ 1; exist = true
        case BuiltinError.EvalErrLoc => evalEx = evalEx+ 1; exist = true
        case BuiltinError.RangeErrLoc => rangeEx = rangeEx+ 1; exist = true
        case BuiltinError.RefErrLoc => refEx = refEx+ 1; exist = true
        case BuiltinError.SyntaxErrLoc => syntaxEx = syntaxEx+ 1; exist = true
        case BuiltinError.TypeErrLoc => typeEx = typeEx+ 1; exist = true
        case BuiltinError.URIErrLoc => uriEx = uriEx+ 1; exist = true
        case _ => ()
      }})
    if (exist) totalEx = totalEx + 1
  }
  
  private def chooseMax(x: Int, y: Int) = if (x > y) x else y
  
  private def locSize(h: Heap, locs: LocSet): Int = {
    if(derefWithoutHeap) locs.size
    else locs.filter((l: Loc) => h(l) </ Obj.bottom).size
  }
  var derefWithoutHeap = false
  def setDerefWithoutHeap(flag: Boolean) = derefWithoutHeap = flag
}
