/*******************************************************************************
    Copyright (c) 2012-2013, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ***************************************************************************** */

package kr.ac.kaist.jsaf.analysis.typing.domain

import scala.collection.immutable.HashSet

object Value {
  /* convenience constructors */
  def apply(v: Loc): Value = Value(PValueBot, LocSet(v))
  def apply(v: LocSet): Value = Value(PValueBot, v)
  def apply(v: PValue): Value = Value(v, LocSetBot)
  def apply(v: AbsUndef): Value = Value(PValue(v), LocSetBot)
  def apply(v: AbsNumber): Value = Value(PValue(v), LocSetBot)
  def apply(v: AbsBool): Value = Value(PValue(v), LocSetBot)
  def apply(v: AbsNull): Value = Value(PValue(v), LocSetBot)
  def apply(v: AbsString): Value = Value(PValue(v), LocSetBot)
}

case class Value(pv: PValue, locs: LocSet) {

  /* partial order */
  def <= (that : Value): Boolean = {
    if (this eq that) true
    else {
      this.pv <= that.pv &&
      this.locs.subsetOf(that.locs)
    }
  }

  /* not a partial order */
  def </ (that: Value): Boolean = {
    if (this eq that) false
    else {
      !(this.pv <= that.pv) ||
      !(this.locs.subsetOf(that.locs))
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
    if (locs(l_r)) Value(pv, (locs - l_r) + l_o)
    else this
  }

  /* weakly substitute l_r by l_o, that is keep l_r together */
  def weakSubsLoc(l_r: Loc, l_o: Loc): Value = {
    if (locs(l_r)) Value(pv, locs + l_o)
    else this
  }

  def typeCount = {
    if (locs.isEmpty)
      pv.typeCount
    else
      pv.typeCount + 1
  }

  def typeKinds: String = {
    val sb = new StringBuilder()
    sb.append(pv.typeKinds)
    if(!locs.isEmpty) sb.append((if(sb.length > 0) ", " else "") + "Object")
    sb.toString
  }
}
