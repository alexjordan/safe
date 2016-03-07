/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString._
import scala.collection.immutable.{HashSet => IHashSet}

// AbsStringPrefSuff is an AbsString that, when the max. set size is reached, 
// keeps thack of the prefix and suffix of the concrete string.
object AbsStringPrefSuff {

  // PSCase models a pair prefix/suffix.
  case class PSCase(pref: String, suff: String) extends AbsString.AbsStringCase

  // Longest Common Prefix.
  def lcp(strs: Set[String]): String =
    strs.foldLeft("")(
      (_, _) => (strs.min.view, strs.max.view).zipped
      .takeWhile(v => v._1 == v._2).unzip._1.mkString
    )
  def lcp(x: String, y: String): String = 
    lcp(Set(x, y))

  // Longest Common Suffix.
  def lcs(strs: Set[String]): String = {
    var r = new IHashSet[String]()
    strs.foreach{ r += _.reverse }
    lcp(r).reverse
  }
  def lcs(x: String, y: String): String = 
    lcs(Set(x, y))
  
  // Construct the proper PSCase object.
  def getCase(strs: Set[String]): AbsString.AbsStringCase = strs.isEmpty match {
    case true 
      => AbsString.StrBotCase
    case false 
      => AbsStringPrefSuff.PSCase(lcp(strs), lcs(strs))
  }

  // Abstraction functions.
  def alpha(str: String): AbsStringPrefSuff = 
    new AbsStringPrefSuff(AbsStringPrefSuff.PSCase(str, str))
  def alpha(strs: IHashSet[String]): AbsStringPrefSuff = 
    new AbsStringPrefSuff(getCase(strs))

  def cast(that: AbsString): AbsStringPrefSuff = that.kind match {
    case StrBotCase
      => new AbsStringPrefSuff(StrBotCase)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case AbsStringPrefSuff.PSCase(p, s)
      => new AbsStringPrefSuff(p, s)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringPrefSuff()
  }  
}

class AbsStringPrefSuff(_kind: AbsStringCase) extends AbsString(_kind) {

  // Constructors.
  def this() = this(AbsStringPrefSuff.PSCase("", ""))
  def this(p: String) = this(AbsStringPrefSuff.PSCase(p, p))
  def this(p: String, s: String) = this(AbsStringPrefSuff.PSCase(p, s))

  override def cast(that: AbsString) = AbsStringPrefSuff.cast(that)
  
  def prefix: String = this.kind match {
    case AbsStringPrefSuff.PSCase(p, _) 
      => p
    case AbsString.StrTopCase
      => ""
    case _ 
      => null
  }

  def suffix: String = this.kind match {
    case AbsStringPrefSuff.PSCase(_, s) 
      => s
    case AbsString.StrTopCase
      => ""
    case _ 
      => null
  }

  override def getSingle: Option[String] = super.getSingle

  override def gamma: Option[Set[String]] = super.gamma

  override def getAbsCase: AbsCase = this.isAllOthers match {
    case true
      => AbsMulti
    case false
      => super.getAbsCase
  }

  override def <= (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringPrefSuff.PSCase(p1, s1), AbsStringPrefSuff.PSCase(p2, s2))
        => p1.length >= p2.length && s1.length >= s2.length &&
           p1.slice(0, p2.length)                     == p2 && 
           s1.slice(s1.length - s2.length, s1.length) == s2
      case _
        => super.<=(cthat)
    }
  }

  override def </ (that: AbsString) = !this.<=(that)

  override def join (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringPrefSuff.PSCase(p1, s1), AbsStringPrefSuff.PSCase(p2, s2))
        => new AbsStringPrefSuff(
             AbsStringPrefSuff.lcp(p1, p2), AbsStringPrefSuff.lcs(s1, s2)
           )
      case _
        => cast(super.join(cthat))
    }
  }

  override def <> (that: AbsString) = cast(super.<>(that))

  override def trim: AbsString = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => new AbsStringPrefSuff(
           p.replaceAll("^\\s+", ""), 
           s.replaceAll("\\s+$", "")
         )
    case _ 
      => cast(super.trim)
  }

  override def concat (that: AbsString) = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringPrefSuff.PSCase(p1, s1), AbsStringPrefSuff.PSCase(p2, s2))
        => new AbsStringPrefSuff(p1, s2)
      case _
        => cast(super.concat(cthat))
    }
  }

  override def charAt(pos: AbsNumber): AbsString = (this.kind, pos.kind) match {
    case (AbsStringPrefSuff.PSCase(p, _), AbsNumber.UIntSingleCase(i))
      => if (0 <= i && i < p.length) {
           val c = p.charAt(i.toInt).toString
           new AbsStringPrefSuff(AbsStringPrefSuff.PSCase(c, c))
         }  
         else
           super.charAt(pos)
    case _
      => super.charAt(pos)
  }
  
  override def charCodeAt(pos: AbsNumber) = (this.kind, pos.kind) match {
    case (AbsStringPrefSuff.PSCase(p, _), AbsNumber.UIntSingleCase(i))
      => if (0 <= i && i < p.length)
           AbsNumber.alpha(p.charAt(i.toInt).toDouble)
         else
           super.charCodeAt(pos)
    case _
      => super.charCodeAt(pos)
  }

  override def contains(that: AbsString) = super.contains(that)
  
  override def length: AbsNumber = super.length

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s) 
      => new AbsStringPrefSuff(p.toLowerCase, s.toLowerCase)
    case _
      => cast(super.toLowerCase)
  }

  // Puts all the characters of this to upper case.
  override def toUpperCase: AbsString = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => new AbsStringPrefSuff(p.toUpperCase, s.toUpperCase)
    case _
      => cast(super.toUpperCase)
  }

  // To abstract string (useful?).
  override def toAbsString: AbsString = this.kind match {
    case AbsStringPrefSuff.PSCase(_, _)
      => this
    case _ 
      => cast(super.toAbsString)
  }

  override def toString: String = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => if (isBottom)
           "Bot"
         else
           if (isTop)
             "String"
           else
             AbsString.format(p) + ".." + AbsString.format(s)
    case _ 
      => super.toString
  }

  override def isTop: Boolean = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => p.isEmpty && s.isEmpty
    case _
      => super.isTop
  }

  override def isBottom: Boolean = super.isBottom

  override def isConcrete: Boolean = super.isConcrete

  override def isEmpty: Boolean = super.isEmpty

  override def isAllNums: Boolean = super.isAllNums

  // FIXME: Improvable with regexp.
  override def isAllOthers: Boolean = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => val n = "0123456789+-xXabcdefABCDEFInfityN".toSet
         if ((p.toSet subsetOf n) && (s.toSet subsetOf n))
           false
         else
           true
    case _
      => super.isAllOthers
  }

  // Object equality.
  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
      case (AbsStringPrefSuff.PSCase(p1, s1), AbsStringPrefSuff.PSCase(p2, s2))
        => p1 == p2 && s1 == s2
      case _
        => this.toString == cast(that).toString
    }
    case _
      => false
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringPrefSuff.PSCase(p, s)
      => "Pref(" + varName + ") = " + p + " /\\ Suff(" + varName + ") =  " + s
    case _
      => super.toConstraint(varName)
  }

  override def matches(s: String): Boolean = AbsStringPrefSuff.alpha(s) <= this
}
