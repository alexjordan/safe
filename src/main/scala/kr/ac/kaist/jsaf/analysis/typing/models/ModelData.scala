/*******************************************************************************
    Copyright (c) 2013, S-Core.
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
