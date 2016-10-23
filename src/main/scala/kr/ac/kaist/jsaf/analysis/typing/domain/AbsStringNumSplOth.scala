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

import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString._

import scala.collection.immutable.{HashSet => IHashSet}
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringNumOth._

object AbsStringNumSplOth {

  case object SplCase       extends AbsString.AbsStringCase
  case object NotSplCase    extends AbsString.AbsStringCase
  case object NotNumSplCase extends AbsString.AbsStringCase

  val splStrings = Set(
    "valueOf", "toString", "length", "constructor", "toLocaleString", "indexOf",
    "hasOwnProperty", "isPrototypeOf", "propertyIsEnumerable", "concat", "join",
    "lastIndexOf", "pop", "push", "reverse", "shift", "sort", "splice"
  )

  def alpha(str: String): AbsStringNumSplOth =
    new AbsStringNumSplOth(getCase(IHashSet(str)))
  def alpha(strs: IHashSet[String]): AbsStringNumSplOth =
    new AbsStringNumSplOth(getCase(strs))

  def getCase(strs: IHashSet[String]): AbsStringCase = {
    val s = strs.intersect(splStrings)
    if (s.isEmpty) {
      val c = AbsStringNumOth.getCase(strs)
      c match {
        case NumStrCase | StrBotCase
          => c
        case OthStrCase
          => NotNumSplCase
        case StrTopCase
          => NotSplCase
      }
    }
    else if (s.size == strs.size)
      SplCase
    else if (strs.exists(x => AbsString.isNum(x)))
      StrTopCase
    else
      OthStrCase
  }

  def cast(that: AbsString): AbsStringNumSplOth = that.kind match {
    case StrBotCase
      => new AbsStringNumSplOth(StrBotCase)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case NumStrCase | OthStrCase | NotSplCase | NotNumSplCase | SplCase
      => new AbsStringNumSplOth(that.kind)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringNumSplOth()
  }

}


class AbsStringNumSplOth(_kind: AbsString.AbsStringCase)
extends AbsStringNumOth(_kind) {

  def this() = this(StrTopCase)

  override def cast(that: AbsString) = AbsStringNumSplOth.cast(that)

  override def getSingle: Option[String] = None

  override def gamma: Option[Set[String]] =
    if (isAllSpecial)
      Some(AbsStringNumSplOth.splStrings)
    else
      None

  override def <=(that: AbsString): Boolean = (this.kind, that.kind) match {
    case (NumStrCase, AbsStringNumSplOth.NotSplCase)
       | (AbsStringNumSplOth.SplCase, OthStrCase)
       | (AbsStringNumSplOth.NotNumSplCase, OthStrCase)
       | (AbsStringNumSplOth.NotNumSplCase, AbsStringNumSplOth.NotSplCase)
      => true
    case _
      => super.<=(cast(that))
  }

  override def </ (that: AbsString): Boolean = !this.<=(that)

  /* join */
  override def join (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this <= cthat, cthat <= this) match {
      case (true, _)
        => cthat
      case (_, true)
        => this
      case _
        => (this.kind, that.kind) match {
             case (NumStrCase, AbsStringNumSplOth.NotNumSplCase)
                | (AbsStringNumSplOth.NotNumSplCase, NumStrCase)
               => new AbsStringNumSplOth(AbsStringNumSplOth.NotSplCase)
             case (AbsStringNumSplOth.SplCase, AbsStringNumSplOth.NotNumSplCase)
                | (AbsStringNumSplOth.NotNumSplCase, AbsStringNumSplOth.SplCase)
               => new AbsStringNumSplOth(OthStrCase)
             case _
               => cast(super.join(cthat))
           }

    }
  }

  /* meet */
  override def <> (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this <= cthat, cthat <= this) match {
      case (true, _)
        => this
      case (_, true)
        => cthat
      case _
      => (this.kind, cthat.kind) match {
           case (OthStrCase, AbsStringNumSplOth.NotSplCase)
              | (AbsStringNumSplOth.NotSplCase, OthStrCase)
             => new AbsStringNumSplOth(AbsStringNumSplOth.NotNumSplCase)
           case _
             => cast(super.<>(cthat))
      }
    }
  }

  override def concat(that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringNumSplOth.SplCase, _)
         | (_, AbsStringNumSplOth.SplCase)
        => new AbsStringNumSplOth(AbsStringNumSplOth.NotNumSplCase)
      case (NumStrCase, _)
         | (_, NumStrCase)
        => new AbsStringNumSplOth(AbsStringNumSplOth.NotSplCase)
      case _
        => cast(super.concat(cthat))
    }
  }

  override def trim: AbsString = this.kind match {
    case AbsStringNumSplOth.NotNumSplCase | AbsStringNumSplOth.NotSplCase
      => new AbsStringNumSplOth(StrTopCase)
    case _
      => cast(super.toLowerCase)
  }

  override def charCodeAt(pos: AbsNumber): AbsNumber = super.charCodeAt(pos)

  override def contains(that: AbsString) = (this.kind, that.kind) match {
    case (AbsStringNumSplOth.SplCase, NumStrCase)
       | (NumStrCase, AbsStringNumSplOth.SplCase)
      => BoolFalse
    case _
      => super.contains(cast(that))
  }

  override def length: AbsNumber = super.length

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringNumSplOth.SplCase
      => new AbsStringNumSplOth(AbsStringNumSplOth.NotNumSplCase)
    case AbsStringNumSplOth.NotNumSplCase
      => new AbsStringNumSplOth(OthStrCase)
    case _
      => cast(super.toLowerCase)
  }

  override def toUpperCase: AbsString = this.kind match {
    case AbsStringNumSplOth.SplCase
      => new AbsStringNumSplOth(AbsStringNumSplOth.NotNumSplCase)
    case _
      => cast(super.toUpperCase)
  }

  override def toString: String = this.kind match {
    case AbsStringNumSplOth.SplCase
      => "SplStr"
    case AbsStringNumSplOth.NotSplCase
      => "NotSplStr"
    case AbsStringNumSplOth.NotNumSplCase
      => "NotNumSplStr"
    case _
      => super.toString
  }

  override def isTop: Boolean = this.kind == AbsString.StrTopCase

  override def isBottom: Boolean = this.kind == AbsString.StrBotCase

  override def isConcrete: Boolean = this.kind == AbsStringNumSplOth.SplCase

  override def toAbsString: AbsString = this.kind match {
    case AbsStringNumSplOth.SplCase
       | AbsStringNumSplOth.NotSplCase
       | AbsStringNumSplOth.NotNumSplCase
      => this
    case _
      => super.toAbsString
  }

  override def isEmpty: Boolean = false

  override def isAllNums: Boolean = super.isAllNums

  override def isAllOthers: Boolean =
    kind == AbsStringNumSplOth.NotNumSplCase || super.isAllOthers

  override def equals(other: Any) = super.equals(other)

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringNumSplOth.SplCase
      => varName + " in " + AbsStringNumSplOth.splStrings
    case AbsStringNumSplOth.NotSplCase
      => varName + " not in " + AbsStringNumSplOth.splStrings
    case AbsStringNumSplOth.NotNumSplCase
      => varName + " not in " + AbsStringNumSplOth.splStrings + " /\\ " +
         "not Number(" + varName + ")"
    case _
      => super.toConstraint(varName)
  }

}
