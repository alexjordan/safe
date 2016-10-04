/**
 * Copyright (c) 2016, Oracle and/or its affiliates.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * Redistributions of source code must retain the above copyright notice, this
 * list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
 * this list of conditions and the following disclaimer in the documentation
 * and/or other materials provided with the distribution.
 * Neither the name of KAIST, S-Core, Oracle nor the names of its contributors
 * may be used to endorse or promote products derived from this software without
 * specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * This distribution may include materials developed by third parties.
 */

package kr.ac.kaist.safe

import kr.ac.kaist.safe.analyzer.domain.DefaultBool.True
import kr.ac.kaist.safe.analyzer.domain._
import org.scalatest.{ BeforeAndAfterAll, FlatSpec }
import org.scalatest.Matchers._
import org.scalatest.Assertions._
import kr.ac.kaist.safe.analyzer.domain.Utils._

/**
 * Created by aljordan on 30/09/16.
 */
class TaintStringTest extends FlatSpec with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    Utils.register(
      DefaultUndef,
      DefaultNull,
      DefaultBool,
      DefaultNumber,
      TaintStringSet(1),
      DefaultLoc,
      NormalAAddr
    )
  }

  "Tainted string" should "behave correctly" in {
    val s = AbsString.Untainted
    val top = AbsString.Top
    s should not be ('isTop)
    s should not be ('isBottom)
    s.getSingle should be('isTop)
    s.gamma should be('isTop)
    s.isNum should be(AbsBool.Top)
    s.toString should equal("Untainted(string)")
    s.toAbsBoolean should be(AbsBool.Top)

    assert(s <= top)
    assert(AbsString.Number <= s)
    assert(AbsString.Other <= s)
    assert(AbsString.Untainted <= s)
    assert(top </ s)

    // preserve untaintedness
    assertResult(AbsString.Untainted) { s + AbsString("foo") }
    assertResult(AbsString.Untainted) { s concat AbsString("foo") }
  }

  "Tainted string" should "behave like StrTop" in {
    val s = AbsString.Untainted
    val top = AbsString.Top

    assert(s.toLowerCase.isTop)
    assert(s.toUpperCase.isTop)

    s.toAbsNumber should equal(AbsNumber.Top)
    s.isArrayIndex should equal(AbsBool.Top)
    s.isRelated("foo") shouldBe true


  }

}
