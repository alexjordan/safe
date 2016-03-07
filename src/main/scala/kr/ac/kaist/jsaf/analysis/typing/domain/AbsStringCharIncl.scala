/*******************************************************************************
  * Copyright (c) 2012-2014, S-Core, KAIST.
  * All rights reserved.
  **
  *Use is subject to license terms.
  **
  *This distribution may include materials developed by third parties.
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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString.{
  StrBotCase, AbsStringCase
}
import scala.collection.immutable.{HashSet => IHashSet}

// AbsStringCharIncl is an AbsString that, when the max. set size is reached, 
// keeps thack of the characters that may/must occur in the concrete string.
object AbsStringCharIncl {

  // Default alphabet (all the Unicode characters).
  var _alphabet = new IHashSet[Char]()
  for (i <- 0 until 256)//65536)
    _alphabet += i.asInstanceOf[Char]
  def alphabet = _alphabet
  def alphabetSize = _alphabet.size

  val numSet = IHashSet[Char]() ++ "0123456789+-xXabcdefABCDEFInfityN".toSet

  // LUCase models an abstract string in terms of a pair of Char sets:
  //   l: lower bound, i.e., the characters that  must belong to the string
  //   u: upper bound, i.e., the characters that might belong to the string
  case class LUCase(l: IHashSet[Char], u: IHashSet[Char]) extends AbsStringCase

  // Construct the proper AbsStringCase object.
  def getCase(l: IHashSet[Char], u: IHashSet[Char]) = l.subsetOf(u) match {
    case true
      => LUCase(l, u)
    case false
      => StrBotCase
  }

  // Abstraction functions.
  def alpha(str: String): AbsStringCharIncl = {
    val s = IHashSet() ++ str.toSet
    new AbsStringCharIncl(LUCase(s, s))
  }
  def alpha(strs: IHashSet[String]): AbsStringCharIncl = {
    if (strs.isEmpty)
      new AbsStringCharIncl(StrBotCase)
    val l = IHashSet() ++ strs.map(x => x.toSet).reduceLeft(_ intersect _)
    val u = IHashSet() ++ strs.map(x => x.toSet).reduceLeft(_ union _)
    new AbsStringCharIncl(getCase(l, u))
  }

  def cast(that: AbsString): AbsStringCharIncl = that.kind match {
    case StrBotCase
      => new AbsStringCharIncl(StrBotCase)
    case LUCase(l, u)
      => new AbsStringCharIncl(l, u)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case AbsStringPrefSuff.PSCase(p, s)
      => new AbsStringCharIncl(IHashSet() ++ p.toSet union s.toSet, alphabet)
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else if (that.isAllNums)
           new AbsStringCharIncl(IHashSet(), numSet)
         else
           new AbsStringCharIncl()
  }

}

class AbsStringCharIncl(_kind: AbsStringCase) extends AbsString(_kind) {

  // Constructors.
  def this() =
    this(AbsStringCharIncl.LUCase(IHashSet(), AbsStringCharIncl.alphabet))
  def this(l: IHashSet[Char]) =
    this(AbsStringCharIncl.LUCase(l, l))
  def this(l: IHashSet[Char], u: IHashSet[Char]) =
    this(AbsStringCharIncl.getCase(l, u))

  override def cast(that: AbsString) = AbsStringCharIncl.cast(that)

  def lower: IHashSet[Char] = this.kind match {
    case AbsStringCharIncl.LUCase(l, _)
      => l
    case AbsString.StrTopCase
      => new IHashSet[Char]()
    case _
      => null
  }

  def upper: IHashSet[Char] = this.kind match {
    case AbsStringCharIncl.LUCase(_, u)
      => u
    case AbsString.StrTopCase
      => AbsStringCharIncl.alphabet
    case _
      => null
  }

  override def getAbsCase: AbsCase = this.kind match {
    case AbsStringCharIncl.LUCase(_, _)
      => if (this.isEmpty)
           AbsSingle
         else if (this.isAllNums || this.isAllOthers)
           AbsMulti
         else
           AbsTop
    case _
      => super.getAbsCase
  }

  override def getSingle: Option[String] = this.kind match {
    case AbsStringCharIncl.LUCase(_, u)
      => if (u.isEmpty)
           Some("")
         else
           None
    case _
      => super.getSingle
  }

  override def gamma: Option[Set[String]] = this.isEmpty match {
    case true
      => Some(Set(""))
    case false
      => None
  }

  override def <= (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => (l2 subsetOf l1) && (u1 subsetOf u2)
      case _
        => super.<=(cthat)
    }
  }

  override def </ (that: AbsString) = !this.<=(that)

  override def join (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => new AbsStringCharIncl(l1 intersect l2, u1 union u2)
      case _
        => cast(super.join(cthat))
    }
  }

  override def <> (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => new AbsStringCharIncl(l1 union l2, u1 intersect u2)
      case _
        => cast(super.<>(cthat))
    }
  }

  override def trim: AbsString = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => if (u.mkString.trim.isEmpty)
           new AbsStringCharIncl(IHashSet(), IHashSet())
         else
           this
    case _ 
      => cast(super.trim)
  }

  override def concat (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => new AbsStringCharIncl(l1 union l2, u1 union u2)
      case _
        => cast(super.concat(cthat))
    }
  }

  override def charAt(pos: AbsNumber): AbsString = this.kind match {
    case AbsStringCharIncl.LUCase(_, u)
      => new AbsStringCharIncl(IHashSet(), u)
    case _
      => super.charAt(pos)
  }

  override def charCodeAt(pos: AbsNumber): AbsNumber = super.charCodeAt(pos)

  override def contains (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => if (!(l2 subsetOf u1))
             BoolFalse
           else
             if (u2.size <= 1 && (u2 subsetOf l1))
               BoolTrue
             else
               BoolTop
      case _
        => super.contains(cthat)
    }
  }

  // Abstract length of this.
  override def length: AbsNumber = this.kind match {
    case AbsStringCharIncl.LUCase(_, u)
      => if (u.isEmpty)
           AbsNumber.alpha(0)
         else
           UInt
    case _
      => super.length
  }

  // Puts all the characters of this to lower case.
  override def toLowerCase: AbsString = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => new AbsStringCharIncl(l.map(x => x.toLower), u.map(x => x.toLower))
    case _
      => cast(super.toLowerCase)
  }

  // Puts all the characters of this to upper case.
  override def toUpperCase: AbsString = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => new AbsStringCharIncl(l.map(x => x.toUpper), u.map(x => x.toUpper))
    case _
      => cast(super.toUpperCase)
  }

  // To abstract string (useful?).
  override def toAbsString: AbsString = this.kind match {
    case AbsStringCharIncl.LUCase(_, _)
      => this
    case _ 
      => cast(super.toAbsString)
  }

  override def toString: String = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => if (l.size == u.size)
           if (u.size == AbsStringCharIncl.alphabetSize)
             "Set(AllChars)"
           else
             "<" + l.toString + ">"
         else
           if (u.size == AbsStringCharIncl.alphabetSize)
             if (l.isEmpty)
               "String"
             else
               "<" + l.toString + ", Set(AllChars)>"
           else
             "<" + l.toString + ", " + u.toString + ">"
    case _ 
      => super.toString
  }

  override def isTop: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => l.isEmpty && u.size == AbsStringCharIncl.alphabetSize
    case _
      => super.isTop
  }

  override def isBottom: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => !(l subsetOf u)
    case _
      => super.isBottom
  }

  override def isConcrete: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => u.isEmpty
    case _
      => super.isConcrete
  }

  override def isEmpty: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => u.isEmpty
    case _
      => super.isEmpty
  }

      // True iff the string is numeric.
  override def isAllNums: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(_, u)
      => u.subsetOf("0123456789".toSet)
    case _
      => super.isAllNums
  }

  // True iff the string is not numeric.
  override def isAllOthers: Boolean = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => l.diff("0123456789xXabcdefABCDEFInfinityNaN+-".toSet).nonEmpty
    case _
      => super.isAllOthers
  }

  // Object equality.
  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
      case (AbsStringCharIncl.LUCase(l1, u1), AbsStringCharIncl.LUCase(l2, u2))
        => l1 == l2 && u1 == u2
      case _
        => this.toString == cast(that).toString
    }
    case _
      => false
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringCharIncl.LUCase(l, u)
      => "Chars(" + varName + ") in (" + l + ", " + u + ")"
    case _
      => super.toConstraint(varName)
  }

  override def matches(s: String): Boolean = AbsStringCharIncl.alpha(s) <= this
}
