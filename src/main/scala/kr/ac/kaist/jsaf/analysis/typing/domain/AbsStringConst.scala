/*******************************************************************************
  * Copyright (c) 2012-2013, S-Core, KAIST.
  * All rights reserved.
  **
  *Use is subject to license terms.
  **
  *This distribution may include materials developed by third parties.
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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString.{
  StrBotCase, StrTopCase, AbsStringCase
}
import scala.collection.immutable.{HashSet => IHashSet}

object AbsStringConst {

  case class StrCase(str: String) extends AbsString.AbsStringCase

  def alpha(str: String): AbsStringConst = new AbsStringConst(StrCase(str))
  def alpha(strs: IHashSet[String]): AbsStringConst =
    new AbsStringConst(getCase(strs))

  def getCase(strs: IHashSet[String]) = strs.size match {
    case 0
      => StrBotCase
    case 1
      => StrCase(strs.head)
    case _
      => StrTopCase
  }

  def cast(that: AbsString): AbsStringConst = that.kind match {
    case StrBotCase
      => new AbsStringConst(StrBotCase)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case AbsStringConst.StrCase(s)
      => new AbsStringConst(that.kind)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringConst()
  }
  
}

class AbsStringConst(_kind: AbsStringCase) extends AbsString(_kind) {

  def this() = this(StrTopCase)
  def this(str: String) = this(AbsStringConst.StrCase(str))

  override def cast(that: AbsString) = AbsStringConst.cast(that)
  
  def string: String = this.kind match {
    case AbsStringConst.StrCase(s)
      => s
    case _
      => null
  }
  
  override def getAbsCase: AbsCase = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsSingle
    case _
      => super.getAbsCase
  }

  override def getSingle: Option[String] = this.kind match {
    case AbsStringConst.StrCase(s)
      => Some(s)
    case _
      => None
  }

  override def gamma: Option[Set[String]] = this.kind match {
    case AbsStringConst.StrCase(s)
      => Some(Set(s))
    case _
      => super.gamma
  }

  /* partial order */
  override def <= (that: AbsString): Boolean =
    super.<=(cast(that))

  /* join */
  override def join (that: AbsString): AbsString =
    cast(super.join(that))

  /* meet */
  override def <> (that: AbsString): AbsString =
    cast(super.<>(that))

  override def trim: AbsString = this.kind match {
    case (AbsStringConst.StrCase(s))
      => AbsStringConst.alpha(s.trim)
    case _
      => cast(super.trim)
  }

  override def concat(that: AbsString): AbsString =
    if (this.isEmpty)
      that
    else if (that.isEmpty)
      this
    else (this.kind, cast(that).kind) match {
      case (AbsStringConst.StrCase(s1), AbsStringConst.StrCase(s2))
        => AbsStringConst.alpha(s1 + s2)
      case _ 
        => cast(super.concat(that))
    }

  override def charAt(pos: AbsNumber): AbsString = (this.kind, pos.kind) match {
    case (AbsStringConst.StrCase(s), AbsNumber.UIntSingleCase(i))
      => if (0 <= i && i < s.length)
           AbsStringConst.alpha(s.charAt(i.toInt).toString)
         else
           AbsStringConst.alpha("")
    case _
      => cast(super.charAt(pos))
  }

  override def charCodeAt(pos: AbsNumber): AbsNumber =
    (this.kind, pos.kind) match {
      case (AbsStringConst.StrCase(s), AbsNumber.UIntSingleCase(i))
        => if (0 <= i && i < s.length)
             AbsNumber.alpha(s.charAt(i.toInt).toDouble)
           else
             AbsNumber.NaN
      case _
        => super.charCodeAt(pos)
  }

  override def contains(that: AbsString): AbsBool =
    (this.kind, cast(that).kind) match {
      case (AbsStringConst.StrCase(s1), AbsStringConst.StrCase(s2))
        => AbsBool.alpha(s1 contains s2)
      case _
        => super.contains(that)
  }

  override def length: AbsNumber = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsNumber.alpha(s.length)
    case _
      => super.length
  }

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsStringConst.alpha(s.toLowerCase)
    case _
      => cast(super.toLowerCase)
  }

  override def toUpperCase: AbsString = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsStringConst.alpha(s.toUpperCase)
    case _
      => cast(super.toUpperCase)
  }

  override def toString: String = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsString.format(s)
    case _
      => super.toString
  }

  override def isTop: Boolean = this.kind == AbsString.StrTopCase
  
  override def isBottom: Boolean = this.kind == AbsString.StrBotCase

  override def isConcrete: Boolean = this.kind match {
    case AbsStringConst.StrCase(_)
    => true
    case _
    => false
  }

  override def toAbsString: AbsString = this.kind match {
    case AbsStringConst.StrCase(_)
      => this
    case _ 
      => new AbsStringConst(StrTopCase)
  }

  override def isEmpty: Boolean = this.kind == AbsStringConst.StrCase("")

  override def isAllNums: Boolean = this.kind match {
    case AbsStringConst.StrCase(s)
      => AbsString.isNum(s)
    case _
      => super.isAllNums
  }

  override def isAllOthers: Boolean = this.kind match {
    case AbsStringConst.StrCase(s)
      => !AbsString.isNum(s)
    case _
      => super.isAllOthers
  }

  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
        case (AbsStringConst.StrCase(s1), AbsStringConst.StrCase(s2))
          => s1 == s2
        case _
          => this.toString == cast(that).toString
      }
    case _
      => false
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringConst.StrCase(s)
      => varName + " = " + AbsString.format(s)
    case _
      => super.toConstraint(varName)
  }

  override def matches(s: String): Boolean = AbsStringConst.alpha(s) <= this
}
