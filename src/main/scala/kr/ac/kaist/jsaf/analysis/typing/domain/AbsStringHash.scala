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

object AbsStringHash {

  val modulus = 64

  def sumChars(s: String) = s.foldLeft[Long](0)((i, c) => i + c.toInt)

  def hash(x: Long) = x % modulus

  case class HashSetCase(x: Long) extends AbsString.AbsStringCase

  def alpha(str: String): AbsStringHash =
    alpha(IHashSet(str))
  def alpha(strs: IHashSet[String]): AbsStringHash =
    new AbsStringHash(getCase(strs))

  def getCase(strs: IHashSet[String]) =
    if (strs.isEmpty)
      StrBotCase
    else
      HashSetCase(strs.foldLeft[Long](0)((i, s) => i | 1L << hash(sumChars(s))))

  def cast(that: AbsString): AbsStringHash = that.kind match {
    case StrBotCase
      => new AbsStringHash(StrBotCase)
    case HashSetCase(h)
      => new AbsStringHash(HashSetCase(h))
    case AbsStringConst.StrCase(s)
      => alpha(s)
    case AbsStringSet.SetCase(s)
      => alpha(s)
    case _
      => if (that.isConcrete)
           alpha(IHashSet() ++ that.gamma.get)
         else
           new AbsStringHash()
  }

}


class AbsStringHash(_kind: AbsStringCase) extends AbsString(_kind) {

  def this() = this(StrTopCase)
  def this(h: Long) = this(AbsStringHash.HashSetCase(h))

  private def toBitSet(h: Long) = java.util.BitSet.valueOf(Array[Long](h))

  override def cast(that: AbsString) = AbsStringHash.cast(that)

  override def getSingle: Option[String] = None

  override def gamma: Option[Set[String]] = None

  /* partial order */
  override def <= (that: AbsString): Boolean = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringHash.HashSetCase(h1), AbsStringHash.HashSetCase(h2))
        => (h1 & ~h2) == 0
      case _
        => super.<=(cthat)
    }
  }

  override def </ (that: AbsString) = !this.<=(that)

  /* join */
  override def join (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringHash.HashSetCase(h1), AbsStringHash.HashSetCase(h2))
        => new AbsStringHash(h1 | h2)
      case _
        => cast(super.join(cthat))
    }
  }

  /* meet */
  override def <> (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringHash.HashSetCase(h1), AbsStringHash.HashSetCase(h2))
        => new AbsStringHash(h1 & h2)
      case _
        => cast(super.<>(cthat))
    }
  }

  override def trim: AbsString = cast(super.trim)


  def concat(a: Long, b: Long): Long = {
    var r: Long = java.lang.Long.reverse(b)
    var c: Long = 0L
    for (i <- 0 until AbsStringHash.modulus) {
      r = java.lang.Long.rotateLeft(r, 1)
      if ((a & r) != 0L)
        c |= (1L << i)
    }
    c
  }

  override def concat(that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringHash.HashSetCase(h1), AbsStringHash.HashSetCase(h2))
        => new AbsStringHash(concat(h1, h2))
      case _
        => cast(super.concat(that))
    }
  }

  override def toString: String = _kind match {
    case AbsStringHash.HashSetCase(h)
      => if (isBottom)
           "StrBot"
         else if (isTop)
           "String"
         else
           "HS(" + toBitSet(h) + ")"
    case _
      => super.toString
  }

  override def isBottom: Boolean = _kind match {
    case AbsString.StrBotCase
       | AbsStringHash.HashSetCase(0L)
      => true
    case _
      => false
  }

  override def isTop: Boolean = _kind match {
    case AbsString.StrTopCase
       | AbsStringHash.HashSetCase(-1L)
    => true
    case _
    => false
  }
  override def isConcrete: Boolean = false

  override def toAbsString: AbsString = _kind match {
    case AbsStringHash.HashSetCase(_)
      => this
    case _
      => new AbsStringHash(StrTopCase)
  }

  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
           case (AbsStringHash.HashSetCase(h1), AbsStringHash.HashSetCase(h2))
             => h1 == h2
           case _
             => this.toString == cast(that).toString
         }
    case _
      => false
  }

  override def matches(s: String): Boolean = AbsStringHash.alpha(s) <= this
}
