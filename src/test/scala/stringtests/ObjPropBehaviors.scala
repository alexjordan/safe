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

package stringtests

import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.{Config, Operator}
import kr.ac.kaist.jsaf.tests.TestHelper._
import org.scalatest._
import org.scalatest.Matchers._
import org.scalatest.OptionValues._
import testhelpers.{AddressManaged, Lockable}
import testhelpers.AbstractMatchers._

trait ObjPropBehaviors {
  this: FlatSpec =>

  val concrete = 'isConcrete
  val bottom = 'isBottom
  val top = 'isTop
  val allNums = 'isAllNums
  val allOthers = 'isAllOthers


  def strAlpha: (String) => AbsString = AbsStringSet.alpha

  def multiCaseStringSet: Unit = {
    it should "support multi case behavior" in {
      val str = AbsString.alpha("123") + AbsString.alpha("foo")
      str should be('isConcrete)
      str should not be ('isTop)
      str.getAbsCase should be(AbsMulti)
      str.gamma.value should contain allOf("foo", "123")
    }
  }

  def saneString: Unit = {
    it should "be bot preserving" in {
      val s: AbsString = StrBot + StrBot
      s.gamma shouldBe None
    }
  }

  def concatenation: Unit = {
    it should "concat concrete" in {
      val s = Operator.bopPlus(toValue("foo"), toValue("bar"))
      s.pv._5.gamma.value should contain only "foobar"
    }
  }
}

// Base class for string test.
// String tests need a lock because setting the string domain or string set size is a global side effect
//
abstract class StringTestBase extends FlatSpec with ObjPropBehaviors with Lockable with AddressManaged

class StringSet1Test extends StringTestBase {
  override def beforeAll() {
    Shell.params.opt_MaxStrSetSize = 1
    super.beforeAll()
  }

  "SAFE default string" should behave like saneString
  it should "hold one concrete string" in {
    val s1 = AbsString.alpha("foo")
    val s2 = AbsString.alpha("bar")
    s1.gamma.value should contain only "foo"
    (s1 + s2).gamma shouldBe None
  }
  it should behave like concatenation
}

class StringSet2Test extends StringTestBase {
  override def beforeAll() {
    Shell.params.opt_MaxStrSetSize = 2
    super.beforeAll()
  }

  "SAFE default string" should behave like saneString
  it should behave like multiCaseStringSet
  it should behave like concatenation
}

