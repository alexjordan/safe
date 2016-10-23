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

object ObjectValue {
  /* convenience constructors */
  def apply(v: AbsNumber, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: AbsUndef, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: AbsString, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: AbsBool, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: PValue, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: AbsNull, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
  def apply(v: Loc, writable: AbsBool, enumerable: AbsBool, configurable: AbsBool): ObjectValue
  = ObjectValue(Value(v),writable, enumerable, configurable)
}

case class ObjectValue(value: Value,
                       writable: AbsBool,
                       enumerable: AbsBool,
                       configurable: AbsBool) {
  /* tuple-like accessor */
  val _1 = value
  val _2 = writable
  val _3 = enumerable
  val _4 = configurable

  /* partial order */
  def <= (that: ObjectValue): Boolean = {
    this.value <= that.value &&
    this.writable <= that.writable &&
    this.enumerable <= that.enumerable &&
    this.configurable <= that.configurable
  }

  /* not a partial order */
  def </ (that: ObjectValue): Boolean = {
    this.value </ that.value ||
    this.writable </ that.writable ||
    this.enumerable </ that.enumerable ||
    this.configurable </ that.configurable
  }

  /* join */
  def + (that: ObjectValue): ObjectValue = {
    ObjectValue(
      this.value + that.value,
      this.writable + that.writable,
      this.enumerable + that.enumerable,
      this.configurable + that.configurable)
  }

  /* meet */
  def <> (that: ObjectValue): ObjectValue = {
    ObjectValue(
      this.value <> that.value,
      this.writable <> that.writable,
      this.enumerable <> that.enumerable,
      this.configurable <> that.configurable)
  }

  def isValid: Boolean = writable != BoolBot
}
