/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
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

import kr.ac.kaist.jsaf.analysis.cfg.FunctionId

object PropValue {
  /* convenience constructors */
  def apply(v: ObjectValue): PropValue = PropValue(v, FunSetBot)
  def apply(v: Value): PropValue = PropValue(ObjectValue(v, BoolBot, BoolBot, BoolBot), FunSetBot)
  def apply(v: AbsString): PropValue = PropValue(ObjectValue(v, BoolBot, BoolBot, BoolBot), FunSetBot)
  def apply(v: AbsNumber): PropValue = PropValue(ObjectValue(v, BoolBot, BoolBot, BoolBot), FunSetBot)
  def apply(v: AbsBool): PropValue = PropValue(ObjectValue(v, BoolBot, BoolBot, BoolBot), FunSetBot)
}

case class PropValue(objval: ObjectValue,
                     funid: FunSet) {
  /* tuple-like accessor */
  val _1 = objval
  val _2 = objval.value
  val _3 = funid

  /* partial order */
  def <= (that: PropValue): Boolean = {
    if (this eq that) true
    else {
      this.objval <= that.objval &&
      this.funid.subsetOf(that.funid)
    }
  }

  /* not a partial order */
  def </ (that: PropValue): Boolean = {
    if (this eq that) false
    else {
      this.objval </ that.objval ||
      !this.funid.subsetOf(that.funid)
    }
  }

  /* join */
  def + (that: PropValue): PropValue = {
    if (this eq that) this
    else
      PropValue(
          this.objval + that.objval,
          this.funid ++ that.funid)
  }

  /* meet */
  def <> (that: PropValue): PropValue = {
    PropValue(
        this.objval <> that.objval,
        this.funid.intersect(that.funid))
  }

  def isValid: Boolean = objval.isValid
}
