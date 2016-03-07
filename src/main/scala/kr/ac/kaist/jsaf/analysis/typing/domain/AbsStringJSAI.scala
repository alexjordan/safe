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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString.{
  StrBotCase, AbsStringCase
}
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringProd.ProdCase
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringNumSplOth.{
  NotNumSplCase, NotSplCase, SplCase, splStrings
}
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringSet.{SetCase, maxSize}
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringNumOth.{
  NumStrCase, OthStrCase
}
import scala.collection.immutable.{HashSet => IHashSet}


object AbsStringJSAI {

  // Abstraction functions.
  def alpha(str: String): AbsStringJSAI =
    alpha(IHashSet(str))
  def alpha(strs: IHashSet[String]): AbsStringJSAI = {
    val doms = new Array[AbsString](2)
    doms(0) = AbsStringNumSplOth.alpha(strs)
    doms(1) = AbsStringSet.alpha(strs)
    new AbsStringJSAI(doms)
  }

  def cast(that: AbsString): AbsStringJSAI = that.kind match {
    case StrBotCase
      => new AbsStringJSAI(StrBotCase)
    case _
      => if (that.isBottom)
           new AbsStringJSAI(StrBotCase)
         else if (that.isConcrete) {
           val s = IHashSet() ++ that.gamma.get
           new AbsStringJSAI(Array(
             AbsStringNumSplOth.alpha(s),
             AbsStringSet      .alpha(s)
           ))
         }
         else
           if (that.isAllSpecial)
             new AbsStringJSAI(Array(
               new AbsStringNumSplOth(SplCase),
               alpha(IHashSet() ++ splStrings
             )))
           else if (that.isAllNums)
             new AbsStringJSAI(Array(
               new AbsStringNumSplOth(NumStrCase),
               new AbsStringSet()
             ))
           else if (that.isAllOthers)
             new AbsStringJSAI(Array(
               new AbsStringNumSplOth(OthStrCase),
               new AbsStringSet()
             ))
           else if (that.isNotSpecial)
             if (that.isAllOthers)
               new AbsStringJSAI(Array(
                 new AbsStringNumSplOth(NotNumSplCase),
                 new AbsStringSet()))
             else
               new AbsStringJSAI(Array(
                 new AbsStringNumSplOth(NotSplCase),
                 new AbsStringSet()))
           else
             new AbsStringJSAI()
  }

}

class AbsStringJSAI(_kind: AbsStringCase) extends AbsStringProd(_kind) {

  def this() =
    this(ProdCase(
      Array(new AbsStringNumSplOth(), new AbsStringSet())
    ))
  protected def this(doms: Array[AbsString]) =
    this(ProdCase(doms))

  override def cast(that: AbsString) = AbsStringJSAI.cast(that)

  override def <= (that: AbsString): Boolean = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (ProdCase(a), ProdCase(b))
        => if (b(1).isConcrete)
             a(1) <= b(1)
           else
             a(0) <= b(0)
      case _
        => super.<=(cthat)
    }
  }

  override def </ (that: AbsString) = !this.<=(that)

  override def join (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (ProdCase(a), ProdCase(b))
        => val sa = a(1).asInstanceOf[AbsStringSet]
           val sb = b(1).asInstanceOf[AbsStringSet]
           var no: AbsStringNumSplOth = null
           var ss: AbsStringSet = null
           (sa.kind, sb.kind) match {
             case (SetCase(s1), SetCase(s2))
               => val s = s1 union s2
                  if (s.size <= maxSize)
                    ss = new AbsStringSet(SetCase(s))
                  else
                    ss = new AbsStringSet()
                  no = AbsStringNumSplOth.alpha(s)
             case _
               => return cast(super.join(cthat))
           }
           new AbsStringJSAI(Array(no, ss))
      case _
        => cast(super.join(cthat))
    }
  }

  override def concat (that: AbsString): AbsString = {
    val cthat = cast(that)
    if (this.isEmpty)
      return cthat
    else
      if (cthat.isEmpty)
        return this
    (this.kind, cthat.kind) match {
      case (ProdCase(a), ProdCase(b))
        => val sa = a(1).asInstanceOf[AbsStringSet]
           val sb = b(1).asInstanceOf[AbsStringSet]
           var no: AbsStringNumSplOth = null
           var ss: AbsStringSet = null
           (sa.kind, sb.kind) match {
             case (SetCase(s1), SetCase(s2))
               => var s = IHashSet[String]()
                  for (x <- s1)
                    for (y <- s2)
                      s += x + y
                  if (s.size <= maxSize)
                    ss = new AbsStringSet(SetCase(s))
                  else
                    ss = new AbsStringSet()
                  no = AbsStringNumSplOth.alpha(s)
             case _
               => return cast(super.concat(cthat))
           }
           new AbsStringJSAI(Array(no, ss))
      case _
        => cast(super.concat(cthat))
    }
  }

  override def toString: String = this.kind match {
    case ProdCase(d)
      => val s = d(1).asInstanceOf[AbsStringSet]
         if (s.isTop)
           d(0).asInstanceOf[AbsStringNumSplOth].toString
         else
           s.toString
    case _
      => super.toString
  }

  override def equals(other: Any) = other match {
    case that: AbsString
      => this.toString == cast(that).toString
    case _
      => false
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case ProdCase(d)
      => val s = d(1).asInstanceOf[AbsStringSet]
         if (s.isTop)
           d(0).asInstanceOf[AbsStringNumSplOth].toConstraint(varName)
         else
           s.toConstraint(varName)
    case _
      => super.toConstraint(varName)
  }

  override def isEmpty = this.kind match {
    case ProdCase(d)
      => d(1).isEmpty
    case _
      => super.isEmpty
  }

  override def <> (that: AbsString): AbsString = cast(super.<>(cast(that)))

  override def trim: AbsString = cast(super.trim)

  override def charAt(pos: AbsNumber): AbsString = cast(super.charAt(pos))

  override def toLowerCase: AbsString = cast(super.toLowerCase)

  override def toUpperCase: AbsString = cast(super.toUpperCase)

  override def toAbsString: AbsString = cast(super.toAbsString)

  override def isAllSpecial: Boolean = domains(0).kind == SplCase

  override def isNotSpecial: Boolean =
    domains(0).kind == NotSplCase || domains(0).kind == NotNumSplCase

}
