/*******************************************************************************
    Copyright (c) 2013, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ***************************************************************************** */

package kr.ac.kaist.jsaf.analysis.typing.models

import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.{SemanticsExpr => SE, AccessHelper => AH, PreHelper, Helper}
import kr.ac.kaist.jsaf.nodes_util.IRFactory

trait ModelData {
  def getInitList(): List[(Loc, List[(String, AbsProperty)])]
  def getSemanticMap(): Map[String, SemanticFun]

  /* helper function for semantics */
  val dummyInfo = IRFactory.makeInfo(IRFactory.dummySpan("Model"))
  protected def getArgValue(h : Heap, ctx: Context, args: CFGExpr, x : String):Value = {
    SE.V(CFGLoad(dummyInfo, args, CFGString(x)), h, ctx)._1
  }
  protected def getArgValueAbs(h : Heap, ctx: Context, args: CFGExpr, s : AbsString):Value = {
    val lset = SE.V(args,h,ctx)._1._2
    val v = lset.foldLeft(ValueBot)((v_1, l) => v_1 + Helper.Proto(h,l,s))
    v
  }

  /**
    * Returns the arguments object if concrete, throws exception otherwise
    * @param h
    * @param ctx
    * @param args
    */
  protected def getConcreteArgsObj(h : Heap, ctx: Context, args: CFGExpr): Obj = {
    val l_args = SE.V(args,h,ctx)._1.locset
    if (l_args.size != 1)
      throw new NonConcreteException("Concrete arguments object expected")
    h(l_args.head)
  }

  protected def getAddrList_use(): LPSet = {
    LPSet(SinglePureLocalLoc, "@env")
  }
}
