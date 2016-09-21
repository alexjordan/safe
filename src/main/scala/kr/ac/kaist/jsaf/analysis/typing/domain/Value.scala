/*******************************************************************************
    Copyright (c) 2012-2013, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
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

import scala.runtime.ScalaRunTime._hashCode
import kr.ac.kaist.jsaf.analysis.imprecision.{ImprecisionHint, ImprecisionTracker, NoHint}


object Value {
  /* convenience constructors */
  def apply(v: Loc): Value =  Value(PValueBot, LocSet(v))
  def apply(v: LocSet): Value =  Value(PValueBot, v)
  def apply(v: PValue): Value =  Value(v, LocSetBot)
  def apply(v: AbsUndef): Value =  Value(PValue(v), LocSetBot)
  def apply(v: AbsNumber): Value =  Value(PValue(v), LocSetBot)
  def apply(v: AbsBool): Value =  Value(PValue(v), LocSetBot)
  def apply(v: AbsNull): Value =  Value(PValue(v), LocSetBot)
  def apply(v: AbsString): Value =  Value(PValue(v), LocSetBot)

  // external use, imprecision is being passed on
  def apply(pv: PValue, locs: LocSet) = new Value(pv, locs, ImprecisionTracker.InstHint)

  def hint(v: Value, hint: ImprecisionHint) { v.imphint = v.imphint + hint }
}

final class Value(private val _pv: PValue, private val _locs: LocSet, var imphint: ImprecisionHint = NoHint)
  extends Product2[PValue,LocSet] {

  // Value is now a Product
  override def _1 = _pv
  override def _2 = _locs
  override def hashCode() = _hashCode(this)
  override def canEqual(that: Any) = that.isInstanceOf[Value]
  override def equals(that: Any) = that match {
    case that: Value => this.hashCode() == that.hashCode()
    case _ => false
  }
  // TODO optimized equals?
//  override def equals(that: Any) = that match {
//    case that: Value if this.pv == that.pv && this.locs == that.locs => that.canEqual(this)
//    case _ => false
//  }

  def locs: LocSet = {
    if (imphint != NoHint)
      ImprecisionTracker.valueLocsAccess(imphint)
    _locs
  }

  def pv: PValue = {
    if (imphint != NoHint)
      ImprecisionTracker.valuePVAccess(imphint)
    _pv
  }

  /* partial order */
  def <= (that : Value): Boolean = {
    if (this eq that) true
    else {
      this._pv <= that._pv &&
      this._locs.subsetOf(that._locs)
    }
  }

  /* not a partial order */
  def </ (that: Value): Boolean = {
    if (this eq that) false
    else {
      !(this._pv <= that._pv) ||
      !(this._locs.subsetOf(that._locs))
    }
  }

  /* join */
  def + (that: Value): Value = {
    if (this eq that) this
    else if (this eq ValueBot) that
    else if (that eq ValueBot) this
    else {
      Value(
        this.pv + that.pv,
        this.locs ++ that.locs)
    }
  }

  /* meet */
  def <> (that: Value): Value = {
    if (this eq that) this
    else {
      Value(
        this.pv <> that.pv,
        this.locs.intersect(that.locs))
    }
  }

  /* substitute l_r by l_o */
  def subsLoc(l_r: Loc, l_o: Loc): Value = {
    if (_locs(l_r)) new Value(_pv, (_locs - l_r) + l_o, imphint)
    else this
  }

  /* weakly substitute l_r by l_o, that is keep l_r together */
  def weakSubsLoc(l_r: Loc, l_o: Loc): Value = {
    if (_locs(l_r)) new Value(pv, locs + l_o, imphint)
    else this
  }

  def typeCount = {
    if (_locs.isEmpty)
      _pv.typeCount
    else
      _pv.typeCount + 1
  }

  def typeKinds: String = {
    val sb = new StringBuilder()
    sb.append(pv.typeKinds)
    if(!_locs.isEmpty) sb.append((if(sb.length > 0) ", " else "") + "Object")
    sb.toString
  }
}
