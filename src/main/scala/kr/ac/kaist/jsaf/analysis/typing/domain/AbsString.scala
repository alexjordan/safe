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

import java.util.{HashMap => JHashMap}
import kr.ac.kaist.jsaf.analysis.imprecision.ImprecisionTracker
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._
import org.apache.commons.lang3.StringEscapeUtils.escapeJava

object AbsString {

  abstract class AbsStringCase
  case object StrTopCase extends AbsStringCase
  case object StrBotCase extends AbsStringCase

  private var alpha_cache: JHashMap[(StrDomainType, String), AbsString] = null

  private val hex  = "(0[xX][0-9a-fA-F]+)".r.pattern
  private val exp  = "[eE][+-]?[0-9]+"
  private val dec1 = "[0-9]+\\.[0-9]*(" +exp+ ")?"
  private val dec2 = "\\.[0-9]+(" +exp+ ")?"
  private val dec3 = "[0-9]+(" +exp+ ")?"
  private val dec  = "([+-]?(Infinity|(" +dec1+ ")|(" +dec2+ ")|(" +dec3+ ")))"
  private val num_regexp = ("NaN|(" +hex+ ")|(" +dec+ ")").r.pattern

  def isHex(str: String): Boolean =
    hex.matcher(str).matches()

  def isNum(str: String): Boolean =
    num_regexp.matcher(str).matches()

  def initCache(): Unit = {
    clearCache()
    alpha_cache = new JHashMap()
  }

  def clearCache(): Unit = {
   if (alpha_cache != null)
      alpha_cache.synchronized {
        alpha_cache.clear()
      }
    alpha_cache = null
  }

  def alpha(strs: Set[String]): AbsString =
    strs.map(x => alpha(x)).foldLeft[AbsString](StrBot)(_ join _)
  def alpha(str: String): AbsString = {

    if (str == null)
      return StrBot

    val sdt = StrDomainDefault
    assert(sdt != null)

    // Check cached result.
    if (alpha_cache != null)
      alpha_cache.synchronized {
        val cached = alpha_cache.get((sdt, str))
        if (cached != null) 
          return cached
      }

    // Compute result if not cached.
    val result = sdt match  {
      case StrDomainSAFE      => AbsStringSAFE     .alpha(str)
      case StrDomainJSAI      => AbsStringJSAI     .alpha(str)
      case StrDomainConst     => AbsStringConst    .alpha(str)
      case StrDomainSet       => AbsStringSet      .alpha(str)
      case StrDomainNumOth    => AbsStringNumOth   .alpha(str)
      case StrDomainAutomata  => AbsStringAutomata .alpha(str)
      case StrDomainCharIncl  => AbsStringCharIncl .alpha(str)
      case StrDomainPrefSuff  => AbsStringPrefSuff .alpha(str)
      case StrDomainNumOthSpl => AbsStringNumSplOth.alpha(str)
      case StrDomainHash      => AbsStringHash     .alpha(str)
      case sd:StrDomainType if sd.length > 1  => AbsStringProd.alpha(str)
    }

    // Cache computed result.
    if (alpha_cache != null)
      alpha_cache.synchronized {
        alpha_cache.put((sdt, str), result)
      }

    result
  }

  def stringFromCase(c: AbsString.AbsStringCase): AbsString = {
    val caseString = new AbsStringCaseWrapper(c)

    StrDomainDefault match {
      // TODO would be nice to clean up these tables
      case StrDomainSAFE      => AbsStringSAFE.cast(caseString)
      case StrDomainJSAI      => AbsStringJSAI.cast(caseString)
      case StrDomainConst     => AbsStringConst.cast(caseString)
      case StrDomainSet       => AbsStringSet.cast(caseString)
      case StrDomainNumOth    => AbsStringNumOth.cast(caseString)
      case StrDomainAutomata  => AbsStringAutomata.cast(caseString)
      case StrDomainCharIncl  => AbsStringCharIncl.cast(caseString)
      case StrDomainPrefSuff  => AbsStringPrefSuff.cast(caseString)
      case StrDomainNumOthSpl => AbsStringNumSplOth.cast(caseString)
      case StrDomainHash      => AbsStringHash.cast(caseString)
      case sd:StrDomainType if sd.length > 1  => AbsStringProd.cast(caseString)
    }
  }

  def format(s: String) = "\"" + escapeJava(s) +"\""

  def fromCharCode(n: AbsNumber): AbsString =
    if (n </ NumBot)
      n.gamma match {
        case Some(vs)
          => vs.foldLeft[AbsString](StrBot)(
               (r, v) => r + AbsString.alpha("%c".format(v.toInt))
             )
        case None
          => StrTop
      }
    else
      StrBot
}

abstract class AbsString(_kind: AbsString.AbsStringCase) extends AbsBase[String] {

  val kind: AbsString.AbsStringCase = _kind

  def cast(that: AbsString) = that

  override def getAbsCase: AbsCase = this.kind match {
    case AbsString.StrBotCase
      => AbsBot
    case _
      => if (this.isAllNums || this.isAllOthers)
           AbsMulti
         else
           AbsTop
  }

  override def getSingle: Option[String] = None

  override def gamma: Option[Set[String]] = None

  /* partial order */
  def <= (that: AbsString): Boolean = (this.kind, that.kind) match {
    case (_, AbsString.StrTopCase)
       | (AbsString.StrBotCase, _)
      => true
    case _
      => that.equals(this)
  }

  /* not a partial order */
  def </ (that: AbsString) = !(this <= that)

  /* join */
  private[domain] def join (that: AbsString): AbsString =
    (this <= that, that <= this) match {
      case (true, _)
        => that
      case (_, true)
        => this
      case _
        => StrTop
    }

  final def + (that: AbsString): AbsString = {
    val result = join(that)
    if (this.isConcrete && that.isConcrete && !result.isConcrete ||
       !this.isTop && !that.isTop && result.isTop)
      ImprecisionTracker.joinLoss(this, that, result)
    result
  }


  /* meet */
  def <> (that: AbsString): AbsString = (this <= that, that <= this) match {
    case (true, _)
      => this
    case (_, true)
      => that
    case _
      => StrBot
  }

  /* abstract operator 'equal to' */
  def === (that: AbsString): AbsBool =
    if (this.isConcrete && that.isConcrete)
      if (this.gamma.get == that.gamma.get)
        BoolTrue
      else
        BoolFalse
    else if (this <= StrBot || that <= StrBot)
      BoolBot
    else
      (this <= that, that <= this) match {
        case (false, false)
          => BoolFalse
        case _
          => BoolTop
      }

  def trim: AbsString = this

  def concat(that: AbsString): AbsString = (this.kind, that.kind) match {
    case (AbsString.StrBotCase, _)
       | (_, AbsString.StrBotCase)
      => StrBot
    case _
      => StrTop
  }

  def charAt(pos: AbsNumber): AbsString = this

  def charCodeAt(pos: AbsNumber): AbsNumber = this.kind match {
    case AbsString.StrBotCase
      => NumBot
    case _
      => UInt + AbsNumber.NaN
  }

  def contains(s: AbsString): AbsBool = this.kind match {
    case AbsString.StrBotCase
      => BoolBot
    case _
      => BoolTop
  }

  def length: AbsNumber = this.kind match {
    case AbsString.StrBotCase
      => NumBot
    case _
      => UInt
  }

  def toLowerCase: AbsString = this

  def toUpperCase: AbsString = this

  override def toString: String = this.kind match {
    case AbsString.StrBotCase
      => "Bot"
    case AbsStringNumOth.NumStrCase
      => "NumStr"
    case AbsStringNumOth.OthStrCase
      => "OtherStr"
    case _
      => "String"
  }

  override def isTop: Boolean = this.kind == AbsString.StrTopCase

  override def isBottom: Boolean = this.kind == AbsString.StrBotCase

  override def isConcrete: Boolean = false

  override def toAbsString: AbsString = StrTop

  def isEmpty: Boolean = false

  def isAllNums: Boolean = this.kind == AbsStringNumOth.NumStrCase

  def isAllOthers: Boolean = this.kind == AbsStringNumOth.OthStrCase

  def isAllSpecial: Boolean = this.kind == AbsStringNumSplOth.SplCase

  def isNotSpecial: Boolean = this.kind == AbsStringNumSplOth.NotSplCase ||
                              this.kind == AbsStringNumSplOth.NotNumSplCase

  override def equals(other: Any) = other match {
    case that: AbsString
      => this.kind == that.kind
    case _
      => false
  }

  def matches(s: String): Boolean

  def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsString.StrBotCase
      => "false"
    case _
      => "true"
  }

}

class AbsStringCaseWrapper(kind: AbsString.AbsStringCase) extends AbsString(kind) {
  // this class only wraps basic abstract string kinds, it does not do anything else
  require(isValid(kind), s"Unexpected kind $kind")

  def isValid(kind: AbsString.AbsStringCase) = kind match {
    case AbsString.StrTopCase | AbsString.StrBotCase => true
    case AbsStringNumOth.NumStrCase | AbsStringNumOth.OthStrCase => true
    case _ => false
  }

  override def matches(s: String): Boolean = throw new AssertionError("Class is not to be used as string")
}
