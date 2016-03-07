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

import kr.ac.kaist.jsaf.Shell._
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString.{
  StrBotCase, StrTopCase
}
import scala.collection.immutable.{HashSet => IHashSet}

object AbsStringSet {

  def maxSize = params.opt_MaxStrSetSize
  def maxSize_=(m: Int): Unit = params.opt_MaxStrSetSize = m

  case class SetCase(values: IHashSet[String]) extends AbsString.AbsStringCase

  def alpha(str: String): AbsStringSet =
    new AbsStringSet(AbsStringSet.SetCase(IHashSet(str)))
  def alpha(strs: IHashSet[String]): AbsStringSet =
    new AbsStringSet(getCase(strs))

  def getCase(strs: IHashSet[String]) =
    if (strs.size <= maxSize)
      if (strs.isEmpty)
        StrBotCase
      else
        AbsStringSet.SetCase(strs)
    else
      StrTopCase

  def cast(that: AbsString): AbsStringSet = that.kind match {
    case StrBotCase
      => new AbsStringSet(StrBotCase)
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case AbsStringSet.SetCase(s)
      => new AbsStringSet(that.kind)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringSet()
  }

}

class AbsStringSet(_kind: AbsString.AbsStringCase) extends AbsString(_kind) {
  
  def this() = this(StrTopCase)
  def this(strs: IHashSet[String]) = this(AbsStringSet.getCase(strs))
  override def cast(that: AbsString) = AbsStringSet.cast(that)

  def strings: IHashSet[String] = this.kind match {
    case AbsStringSet.SetCase(s)
      => s
    case _
      => null
  }

  override def getAbsCase: AbsCase = this.kind match {
    case AbsStringSet.SetCase(s)
      => if (s.size == 1)
           AbsSingle
         else
           AbsMulti
    case _
      => super.getAbsCase
  }

  override def getSingle: Option[String] = this.kind match {
    case AbsStringSet.SetCase(s)
      => if (s.size == 1)
           Some(s.head)
         else
           None
    case _
      => None
  }

  override def gamma: Option[Set[String]] = this.kind match {
    case AbsStringSet.SetCase(s)
      => Some(s)
    case _
      => super.gamma
  }

  /* partial order */
  override def <= (that: AbsString): Boolean = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
        => s1 subsetOf s2
      case _
        => super.<=(cthat)
    }
  }

  override def </ (that: AbsString) = !this.<=(that)

  /* join */
  override def join (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
        => AbsStringSet.alpha(s1 union s2)
      case _
        => cast(super.join(cthat))
    }
  }

  /* meet */
  override def <> (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
        => AbsStringSet.alpha(s1 intersect s2)
      case _
        => cast(super.<>(cthat))
    }
  }

  override def trim: AbsString = this.kind match {
    case (AbsStringSet.SetCase(s))
      => AbsStringSet.alpha(s.map(x => x.trim))
    case _
      => cast(super.trim)
  }

  override def concat(that: AbsString): AbsString =
    if (this.isEmpty)
      that
    else if (that.isEmpty)
      this
    else {
      val cthat = cast(that)
      (this.kind, cthat.kind) match {
        case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
          => if (s1.size * s2.size <= AbsStringSet.maxSize) {
               var s = IHashSet[String]()
               for (x <- s1)
                 for (y <- s2)
                   s += x + y
               AbsStringSet.alpha(s)
             }
             else
               new AbsStringSet(StrTopCase)
        case _
          => cast(super.concat(that))
      }
    }

  override def charAt(pos: AbsNumber): AbsString = (this.kind, pos.kind) match {
    case (AbsStringSet.SetCase(s), AbsNumber.UIntSingleCase(i))
      => var c = IHashSet[String]()
         for (x <- s)
           if (0 <= i && i < x.length)
             c += x.charAt(i.toInt).toString
           else
             c += ""
         AbsStringSet.alpha(c)
    case _
      => cast(super.charAt(pos))
  }

  override def charCodeAt(pos: AbsNumber): AbsNumber =
    (this.kind, pos.kind) match {
      case (AbsStringSet.SetCase(s), AbsNumber.UIntSingleCase(i))
        => var n = IHashSet[Double]()
           var nan = false
           for (x <- s)
             if (0 <= i && i < x.length)
               n += x.charAt(i.toInt).toDouble
             else
               nan = true
           if (n.size == 1)
             AbsNumber.alpha(n.head)
           else
             if (nan)
               AbsNumber.NaN + AbsNumber.UInt
             else
               AbsNumber.UInt
      case _
        => super.charCodeAt(pos)
    }

  override def contains(that: AbsString): AbsBool = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
        => var forall = true
           var exists = false
           for (x <- s1)
             for (y <- s2) {
               val b = x contains y
               forall &&= b
               exists ||= b
             }
           if (forall)
             if (exists)
               BoolTrue
             else
               BoolBot
           else
             if (exists)
               BoolTop
             else
               BoolFalse
      case _
        => super.contains(cthat)
    }
  }


  override def length: AbsNumber = this.kind match {
    case AbsStringSet.SetCase(s)
      => val l = s.map(x => x.length)
         val m = l.min
         if (m == l.max)
           AbsNumber.alpha(m.toDouble)
         else
           UInt
    case _
      => super.length
  }

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringSet.SetCase(s)
      => AbsStringSet.alpha(s.map(x => x.toLowerCase))
    case _
      => cast(super.toLowerCase)
  }

  override def toUpperCase: AbsString = this.kind match {
    case AbsStringSet.SetCase(s)
      => AbsStringSet.alpha(s.map(x => x.toUpperCase))
    case _
      => cast(super.toUpperCase)
  }

  override def toString: String = this.kind match {
    case AbsStringSet.SetCase(s)
      =>  var t = s.foldLeft("")((result, x) =>
            if (result.length == 0)
              AbsString.format(x)
            else
              result + ", " + AbsString.format(x)
          )
          if (s.size > 1)
            t = "{" + t + "}"
          t
    case _
      => super.toString
  }

  override def isTop: Boolean = this.kind == AbsString.StrTopCase

  override def isBottom: Boolean = this.kind == AbsString.StrBotCase

  override def isConcrete: Boolean = this.kind match {
    case AbsStringSet.SetCase(_)
      => true
    case _
      => false
  }

  override def toAbsString: AbsString = this.kind match {
    case AbsStringSet.SetCase(_)
      => this
    case _
      => new AbsStringSet(StrTopCase)
  }

  override def isEmpty: Boolean = this.kind match {
    case AbsStringSet.SetCase(s)
      => s.size == 1 && s.head == ""
    case _
      => false
  }

  override def isAllNums: Boolean = this.kind match {
    case AbsStringSet.SetCase(s)
      => for (x <- s)
           if (!AbsString.isNum(x))
             return false
         true
    case _
      => super.isAllNums
  }

  override def isAllOthers: Boolean = this.kind match {
    case AbsStringSet.SetCase(s)
      => for (x <- s)
           if (AbsString.isNum(x))
             return false
         true
    case _
      => super.isAllOthers
  }

  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
            case (AbsStringSet.SetCase(s1), AbsStringSet.SetCase(s2))
              => s1 == s2
            case _
              => this.toString == cast(that).toString
         }
    case _
      => false
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringSet.SetCase(s)
      => s.foldLeft("")(
           (c, x) => if (c.length == 0)
             varName + " = " + AbsString.format(x)
           else
             c + " \\/ " + AbsString.format(x)
         )
    case _
      => super.toConstraint(varName)
  }

  override def matches(s: String): Boolean = this.gamma match {
    case Some(xs) => xs.contains(s)
    case None => this.kind match {
      case StrTopCase => true
      case StrBotCase => false
    }
  }
}
