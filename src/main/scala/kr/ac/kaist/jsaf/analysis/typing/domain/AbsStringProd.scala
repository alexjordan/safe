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

import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._

import scala.collection.immutable.{HashSet => IHashSet}
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsString._
import kr.ac.kaist.jsaf.analysis.typing.domain.AbsStringNumOth.{NumStrCase, OthStrCase}

object AbsStringProd {

  case class ProdCase(doms: Array[AbsString]) extends AbsStringCase

  // Abstraction functions.
  def alpha(str: String): AbsStringProd = alpha(IHashSet(str))
  def alpha(strs: IHashSet[String]) = new AbsStringProd(getCase(strs))

  def getCase(strs: IHashSet[String], std: StrDomainType = StrDomainDefault) = {
    val n = std.length
    if (n > 0) {
      val doms = new Array[AbsString](n)
      for (i <- 0 until n)
        Seq(std(i)) match {
          case StrDomainSAFE      => doms(i) = AbsStringSAFE     .alpha(strs)
          case StrDomainJSAI      => doms(i) = AbsStringJSAI     .alpha(strs)
          case StrDomainConst     => doms(i) = AbsStringConst    .alpha(strs)
          case StrDomainSet       => doms(i) = AbsStringSet      .alpha(strs)
          case StrDomainNumOth    => doms(i) = AbsStringNumOth   .alpha(strs)
          case StrDomainAutomata  => doms(i) = AbsStringAutomata .alpha(strs)
          case StrDomainCharIncl  => doms(i) = AbsStringCharIncl .alpha(strs)
          case StrDomainPrefSuff  => doms(i) = AbsStringPrefSuff .alpha(strs)
          case StrDomainNumOthSpl => doms(i) = AbsStringNumSplOth.alpha(strs)
          case StrDomainHash      => doms(i) = AbsStringHash     .alpha(strs)
        }
      ProdCase(doms)
    }
    else
      StrBotCase
  }

  def newDoms = {
    val domain_types: StrDomainType = StrDomainDefault
    val n = domain_types.length
    val doms = new Array[AbsString](n)
    for (i <- 0 until n)
      Seq(domain_types(i)) match {
        case StrDomainSAFE      => doms(i) = new AbsStringSAFE()
        case StrDomainJSAI      => doms(i) = new AbsStringJSAI()
        case StrDomainConst     => doms(i) = new AbsStringConst()
        case StrDomainSet       => doms(i) = new AbsStringSet()
        case StrDomainNumOth    => doms(i) = new AbsStringNumOth()
        case StrDomainAutomata  => doms(i) = new AbsStringAutomata()
        case StrDomainCharIncl  => doms(i) = new AbsStringCharIncl()
        case StrDomainPrefSuff  => doms(i) = new AbsStringPrefSuff()
        case StrDomainNumOthSpl => doms(i) = new AbsStringNumSplOth()
        case StrDomainHash      => doms(i) = new AbsStringHash()
      }
    doms
  }

  def numDoms = {
    val InternalNumStr = new AbsStringCaseWrapper(NumStrCase)
    val domain_types: StrDomainType = StrDomainDefault
    val n = domain_types.length
    val doms = new Array[AbsString](n)
    for (i <- 0 until n)
      Seq(domain_types(i)) match {
        case StrDomainSAFE      => doms(i) =
          AbsStringSAFE.cast(InternalNumStr)
        case StrDomainJSAI      => doms(i) =
          AbsStringJSAI.cast(InternalNumStr)
        case StrDomainConst     => doms(i) = new AbsStringConst()
        case StrDomainSet       => doms(i) = new AbsStringSet()
        case StrDomainNumOth    => doms(i) =
          new AbsStringNumOth(AbsStringNumOth.NumStrCase)
        case StrDomainAutomata  => doms(i) = new AbsStringAutomata()
        case StrDomainCharIncl  => doms(i) =
          AbsStringCharIncl.cast(InternalNumStr)
        case StrDomainPrefSuff  => doms(i) = new AbsStringPrefSuff()
        case StrDomainNumOthSpl => doms(i) =
          new AbsStringNumSplOth(AbsStringNumOth.NumStrCase)
        case StrDomainHash      => doms(i) = new AbsStringHash()
      }
    doms
  }

  def othDoms = {
    val InternalOtherStr = new AbsStringCaseWrapper(OthStrCase)
    val domain_types: StrDomainType = StrDomainDefault
    val n = domain_types.length
    val doms = new Array[AbsString](n)
    for (i <- 0 until n)
      Seq(domain_types(i)) match {
        case StrDomainSAFE      => doms(i) =
          AbsStringSAFE.cast(InternalOtherStr)
        case StrDomainJSAI      => doms(i) =
          AbsStringJSAI.cast(InternalOtherStr)
        case StrDomainConst     => doms(i) = new AbsStringConst()
        case StrDomainSet       => doms(i) = new AbsStringSet()
        case StrDomainNumOth    => doms(i) =
          new AbsStringNumOth(AbsStringNumOth.OthStrCase)
        case StrDomainAutomata  => doms(i) = new AbsStringAutomata()
        case StrDomainCharIncl  => doms(i) = new AbsStringCharIncl()
        case StrDomainPrefSuff  => doms(i) = new AbsStringPrefSuff()
        case StrDomainNumOthSpl => doms(i) =
          new AbsStringNumSplOth(AbsStringNumOth.OthStrCase)
        case StrDomainHash      => doms(i) = new AbsStringHash()
      }
    doms
  }
  
  def cast(that: AbsString): AbsStringProd = that.kind match {
    case StrBotCase
      => new AbsStringProd(StrBotCase)
    case StrTopCase
      => new AbsStringProd()
    case ProdCase(d)
      => if (!d.exists(x => x.isBottom))
           new AbsStringProd(d)
         else
           new AbsStringProd(StrBotCase)
    case _
      => if (that.isBottom)
           new AbsStringProd(StrBotCase)
         else if (that.isAllNums)
           new AbsStringProd(numDoms)
         else if (that.isAllOthers)
           new AbsStringProd(othDoms)
         else
           new AbsStringProd(Array(that))
  }

}

// TODO: Make the methods short-circuited.
class AbsStringProd(_kind: AbsStringCase) extends AbsString(_kind) {

  def this(doms: Array[AbsString]) = this(AbsStringProd.ProdCase(doms))
  def this() = this(AbsStringProd.newDoms)

  override def cast(that: AbsString) = AbsStringProd.cast(that)

  def domains: Array[AbsString] = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d
    case _
      => null
  }

  override def getAbsCase: AbsCase = {
    val res = this.kind match {
      case AbsStringProd.ProdCase(d)
      => if (d.map(x => x.getAbsCase == AbsSingle).reduceLeft(_ || _))
        AbsSingle
      else if (d.map(x => x.getAbsCase == AbsMulti).reduceLeft(_ || _))
        AbsMulti
      else
        super.getAbsCase
      case _
      => super.getAbsCase
    }
    DomainPrinter.gammaPrint(this, res.toString)
    res
  }

  override def getSingle: Option[String] = this.kind match {
    case AbsStringProd.ProdCase(d)
      => for (x <- d) {
           val s = x.getSingle
           if (s.isDefined)
             return Some(s.get)
         }
         None
    case _
      => super.getSingle
  }

  override def gamma: Option[Set[String]] = {
    val res = this.kind match {
      case AbsStringProd.ProdCase(d)
        => var g: Option[Set[String]] = None
          for (x <- d) {
            val s = x.gamma
            if (s.isDefined)
              if (g.isDefined)
                g = Some(g.get.intersect(s.get))
              else
                g = Some(s.get)
          }
          g
      case _
        => super.gamma
    }
    DomainPrinter.gammaPrint(this, res.toString)
    res
  }

  override def <= (that: AbsString) = {
    val cthat = cast(that)
    val res = (this.kind, cthat.kind) match {
      case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
        => (a, b).zipped.map(_ <= _).reduceLeft(_ && _)
      case _
        => super.<=(cthat)
    }
    DomainPrinter.precedesPrint(this, that, res)
    res
  }

  override def </ (that: AbsString) = !this.<=(that)

  override def join (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
        => new AbsStringProd(AbsStringProd.ProdCase((a,b).zipped.map(_ join _)))
      case _
        => cast(super.join(cthat))
    }
  }

  override def <> (that: AbsString): AbsString = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
        => val c = (a, b).zipped.map(_ <> _)
           if (!c.exists(x => x.isBottom))
             new AbsStringProd(c)
           else
             new AbsStringProd(StrBotCase)
      case _
        => cast(super.<>(cthat))
    }
  }

  override def trim: AbsString = this.kind match {
    case (AbsStringProd.ProdCase(d))
    => new AbsStringProd(d.map(x => x.trim))
    case _
    => cast(super.trim)
  }

  override def concat(that: AbsString): AbsString =
    if (this.isEmpty)
      cast(that)
    else if (that.isEmpty)
      this
    else {
      val cthat = cast(that)
      (this.kind, cthat.kind) match {
        case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
          => new AbsStringProd(
               AbsStringProd.ProdCase((a, b).zipped.map(_ concat _))
             )
        case _
          => cast(super.concat(that))
      }
    }

  override def charAt(pos: AbsNumber): AbsString = (this.kind, pos.kind) match {
    case (AbsStringProd.ProdCase(d), _)
      => new AbsStringProd(d.map(x => x charAt pos))
    case _
      => cast(super.charAt(pos))
  }

  override def charCodeAt(pos: AbsNumber): AbsNumber =
    (this.kind, pos.kind) match {
      case (AbsStringProd.ProdCase(d), _)
        => d.map(x => x charCodeAt pos).reduceLeft(_ <> _)
      case _
        => super.charCodeAt(pos)
    }

  override def contains(that: AbsString): AbsBool = {
    val cthat = cast(that)
    (this.kind, cthat.kind) match {
      case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
        => (a, b).zipped.map(_ contains _).reduceLeft(_ <> _)
      case _
        => super.contains(cthat)
    }
  }

  override def length: AbsNumber = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.length).reduceLeft(_ <> _)
    case _
      => super.length
  }

  override def toLowerCase: AbsString = this.kind match {
    case AbsStringProd.ProdCase(d)
    => new AbsStringProd(d.map(x => x.toLowerCase))
    case _
    => cast(super.toLowerCase)
  }

  override def toUpperCase: AbsString = this.kind match {
    case AbsStringProd.ProdCase(d)
    => new AbsStringProd(d.map(x => x.toUpperCase))
    case _
    => cast(super.toUpperCase)
  }

  override def toString: String = this.kind match {
    case AbsStringProd.ProdCase(d)
      => if (isBottom)
           "Bot"
         else if (isTop)
           "String"
         else if (isConcrete)
           AbsStringSet.alpha(IHashSet() ++ gamma.get).toString
         else
           "(" + d.foldLeft("")((result, x) =>
           if (result.length == 0)
             x.toString
           else
             result + ", " + x.toString
           ) + ")"
    case _
      => super.toString
  }

  override def isTop: Boolean = this.kind match  {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isTop).reduceLeft(_ && _)
    case _
      => super.isTop
  }
  override def isBottom: Boolean = this.kind match  {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isBottom).reduceLeft(_ || _)
    case _
      => super.isBottom
  }

  override def isConcrete: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isConcrete).reduceLeft(_ || _)
    case _
      => super.isConcrete
  }

  override def toAbsString: AbsString = this.kind match {
    case AbsStringProd.ProdCase(_)
      => this
    case _
      => new AbsStringProd(StrTopCase)
  }

  override def isEmpty: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isEmpty).reduceLeft(_ || _)
    case _
      => super.isEmpty
  }

  override def isAllNums: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isAllNums).reduceLeft(_ || _)
    case _
      => super.isAllNums
  }

  override def isAllOthers: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.map(x => x.isAllOthers).reduceLeft(_ || _)
    case _
      => super.isAllOthers
  }

  override def isAllSpecial: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
    => d.map(x => x.isAllSpecial).reduceLeft(_ || _)
    case _
    => super.isAllOthers
  }

  override def isNotSpecial: Boolean = this.kind match {
    case AbsStringProd.ProdCase(d)
    => d.map(x => x.isNotSpecial).reduceLeft(_ || _)
    case _
    => super.isAllOthers
  }

  override def equals(other: Any) = other match {
    case that: AbsString
      => (this.kind, cast(that).kind) match {
         case (AbsStringProd.ProdCase(a), AbsStringProd.ProdCase(b))
           => (a, b).zipped.map(_ equals _).reduceLeft(_ && _) ||
               this.toString == cast(that).toString
         case _
           => this.toString == cast(that).toString
         }
    case _
      => false
  }

  override def matches(s: String): Boolean = this.kind match {
    case AbsStringProd.ProdCase(pc) => pc.forall(_.matches(s))
    case NumStrCase | OthStrCase | StrTopCase => {
      val a = AbsStringProd.alpha(s)
      a <= this
    }
  }

  override def toConstraint(varName: String = "X"): String = this.kind match {
    case AbsStringProd.ProdCase(d)
      => d.foldLeft("")(
           (c, x) => if (c.length == 0)
             varName + " = " + AbsString.format(x.toConstraint(varName))
           else
             c + " /\\ " + AbsString.format(x.toConstraint(varName))
         )
    case _
      => super.toConstraint(varName)
  }

}
