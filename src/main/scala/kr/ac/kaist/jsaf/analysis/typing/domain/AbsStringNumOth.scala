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
  StrBotCase, StrTopCase, AbsStringCase, isNum
}
import scala.collection.immutable.{HashSet => IHashSet}

object AbsStringNumOth {

  case object NumStrCase extends AbsStringCase
  case object OthStrCase extends AbsStringCase

  def alpha(str: String): AbsStringNumOth =
    new AbsStringNumOth(getCase(IHashSet(str)))
  def alpha(strs: IHashSet[String]): AbsStringNumOth =
    new AbsStringNumOth(getCase(strs))

  def getCase(strs: IHashSet[String]): AbsStringCase = {
    var num = false
    var oth = false
    for (s <- strs)
      if (isNum(s)) {
        if (oth)
          return StrTopCase
        num = true
      }
      else {
        if (num)
          return StrTopCase
        oth = true
      }
    if (num)
      NumStrCase
    else
      if (oth)
        OthStrCase
      else
        StrBotCase
  }

  def cast(that: AbsString): AbsStringNumOth = that.kind match {
    case StrBotCase
      => new AbsStringNumOth(StrBotCase)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case NumStrCase
       | OthStrCase
      => new AbsStringNumOth(that.kind)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringNumOth()
  }

}

class AbsStringNumOth(_kind: AbsStringCase) extends AbsString(_kind) {

  def this() = this(StrTopCase)

  override def cast(that: AbsString) = AbsStringNumOth.cast(that)

  override def getSingle: Option[String] = None

  override def gamma: Option[Set[String]] = super.gamma

  override def <= (that: AbsString): Boolean = super.<=(cast(that))

  override def </ (that: AbsString): Boolean = !this.<=(that)

  /* join */
  override def join (that: AbsString): AbsString = cast(super.join(cast(that)))

  /* meet */
  override def <> (that: AbsString): AbsString = cast(super.<>(cast(that)))

  override def trim: AbsString = this.kind match {
    case AbsStringNumOth.OthStrCase
      => new AbsStringNumOth(StrTopCase)
    case _
      => this
  }

  override def concat(that: AbsString) = cast(super.concat(that))

  override def charCodeAt(pos: AbsNumber): AbsNumber = super.charCodeAt(pos)

  override def contains(that: AbsString): AbsBool = super.contains(cast(that))

  override def length: AbsNumber = super.length

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringNumOth.NumStrCase
      => new AbsStringNumOth(StrTopCase)
    case _
      => this
  }

  override def toUpperCase: AbsString = this.kind match {
    case AbsStringNumOth.NumStrCase
      => new AbsStringNumOth(StrTopCase)
    case _
      => this
  }

  override def toString: String = this.kind match {
    case AbsStringNumOth.NumStrCase
      => "NumStr"
    case AbsStringNumOth.OthStrCase
      => "OtherStr"
    case _
      => super.toString
  }

  override def isTop: Boolean = this.kind == StrTopCase

  override def isBottom: Boolean = this.kind == StrBotCase

  override def isConcrete: Boolean = false

  override def toAbsString: AbsString = this.kind match {
    case AbsStringNumOth.NumStrCase
       | AbsStringNumOth.OthStrCase
      => this
    case _
      => new AbsStringNumOth()
  }

  override def isEmpty: Boolean = false

  override def isAllNums: Boolean = super.isAllNums

  override def isAllOthers: Boolean = super.isAllOthers

  override def equals(other: Any) = super.equals(other)

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringNumOth.NumStrCase
      => "Number(" + varName + ")"
    case AbsStringNumOth.OthStrCase
      => "Other(" + varName + ")"
    case _
      => super.toConstraint(varName)
  }

  override def matches(s: String): Boolean = AbsStringNumOth.alpha(s) <= this
}
