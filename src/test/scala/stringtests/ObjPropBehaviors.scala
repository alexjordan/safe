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
import kr.ac.kaist.jsaf.analysis.typing.{Operator, PreConfig}
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._
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

  // TODO based on load15.js test, needs to be refactored
  def load15: Unit = {
    it should "support lookup when StringTop" in {
      var o = Obj.empty
      o = o.update(AbsString.alpha("1"), makePropVal(toValue(123)))
      o("1")._2 should absEq(toValue(123))
      val str = AbsString.alpha("123") + AbsString.alpha("foo")
      str should not be concrete
      str should not be bottom
      str shouldBe top
      str.gamma should be(None)
      str.getAbsCase should be(AbsTop)
      o(str)._2 should absEq(toValue(123))
    }
  }

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

    it should "support an empty string" in {
      val e = AbsString.alpha("")
      e shouldBe 'empty
      e shouldBe 'concrete
      e.getAbsCase shouldBe AbsSingle
      e.getSingle.value shouldBe ""
    }
  }

  def noInformationNonConcrete(s: AbsString): Unit = {
    it should "behave like it knows nothing concrete about its string" in {
      s should not be concrete
      s should not be allOthers
      s should not be allNums
      s.gamma shouldBe None
      s.getAbsCase shouldBe AbsTop
    }
  }

  def partialInformationNonConcrete(s: AbsString): Unit = {
    it should "behave like it knows only num/other about this string" in {
      s should not be concrete
      s.gamma shouldBe None
      s.getAbsCase shouldBe AbsMulti
      List(s.isAllOthers, s.isAllNums) should contain allOf (true, false)
    }
  }

  def nonPreservingConcat: Unit = {
    it should "lose num-string knowledge on concat" in {
      val s1 = AbsString.alpha("123")
      val s2 = Operator.bopPlus(Value(s1), Value(NumTop))._1._5

      s1 shouldBe allNums

      s2 should not be allOthers
      s2 should not be concrete
      s2 should not be allNums
      s2.getAbsCase shouldBe AbsTop
    }

    it should "lose other-string knowledge on concat" in {
      val s1 = AbsString.alpha("abc")
      val s2 = Operator.bopPlus(Value(s1), Value(StrTop))._1._5

      s1 shouldBe allOthers

      s2 should not be allOthers
      s2 should not be concrete
      s2 should not be allNums
      s2.getAbsCase shouldBe AbsTop
    }
  }

  def concatenation: Unit = {
    it should "concat concrete" in {
      val s = Operator.bopPlus(toValue("foo"), toValue("bar"))
      s._1._5.gamma.value should contain only "foobar"
    }
  }
}

// Base classes for string test.
// String tests need a lock because setting the string domain or string set size is a global side effect
//
abstract class StringTestBase extends FlatSpec with ObjPropBehaviors with Lockable {
  def stringSetup(): Unit
  override def beforeAll(): Unit = {
    Shell.params.opt_force_strdom = true // all tests are forced on their string domain
    super.beforeAll() // propagate beforeAll setup
    stringSetup() // tests setup the string domain to be tested
  }
}
abstract class HighLevelStringTest extends StringTestBase with AddressManaged

class ReconfigureDomainTest extends StringTestBase {
  override def stringSetup() {
    AbsStringSet.maxSize = 1
    PreConfig.strings = TestStringConfig(StrDomainSet)
  }

  s"String domain test config" should "support reconfiguration" in {
    PreConfig.strings = TestStringConfig(StrDomainSet)
    AbsString.alpha("") shouldBe a [AbsStringSet]
    PreConfig.strings = TestStringConfig(StrDomainCharIncl)
    AbsString.alpha("") shouldBe a [AbsStringCharIncl]
    PreConfig.strings = TestStringConfig(StrDomainSet ++ StrDomainCharIncl)
    AbsString.alpha("") shouldBe a [AbsStringProd]
  }
}

class StringSSTest extends StringTestBase {
  override def stringSetup() {
    AbsStringSet.maxSize = 1
    PreConfig.strings = TestStringConfig(StrDomainSet)
  }

  s"String set (${AbsStringSet.maxSize})" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringSet]
  }
  it should behave like load15
  it should behave like saneString
}

class StringSS2Test extends StringTestBase {
  override def stringSetup() {
    AbsStringSet.maxSize = 2
    PreConfig.strings = TestStringConfig(StrDomainSet)
  }

  s"String set (${AbsStringSet.maxSize})" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringSet]
  }
  it should behave like multiCaseStringSet
}

class StringNoSSTest extends StringTestBase {
  override def stringSetup() {
    AbsStringSet.maxSize = 1
    PreConfig.strings = TestStringConfig(StrDomainSAFE)
  }

  s"NumOth + StringSet (${AbsStringSet.maxSize})" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringSAFE]
  }
  it should behave like load15
}

class StringPSTest extends StringTestBase {
  override def stringSetup() {
    PreConfig.strings = TestStringConfig(StrDomainPrefSuff)
  }

  val nums = "123"
  val chars = "abc"
  s"Prefix-Suffix $nums" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringPrefSuff]
  }
  it should behave like noInformationNonConcrete(AbsStringPrefSuff.alpha(nums))
  s"Prefix-Suffix $chars" should behave like noInformationNonConcrete(AbsStringPrefSuff.alpha(chars))
}

class StringCITest extends StringTestBase {
  override def stringSetup() {
    PreConfig.strings = TestStringConfig(StrDomainCharIncl)
  }

  "Character inclusion" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringCharIncl]
  }
  val nums = AbsStringCharIncl.alpha("123")
  val chars = AbsStringCharIncl.alpha("zrt")
  it should behave like saneString
  s"Character inclusion $nums" should behave like partialInformationNonConcrete(nums)
  s"Character inclusion $chars" should behave like partialInformationNonConcrete(chars)
  s"Character inclusion ${nums + chars}" should behave like noInformationNonConcrete(nums + chars)
}

class StringPSxSSTest extends HighLevelStringTest {
  override def stringSetup() {
    AbsStringSet.maxSize = 1
    PreConfig.strings = TestStringConfig(StrDomainPrefSuff ++ StrDomainSet)
  }

  "Prefix-Suffix x StringSet" should "support alpha" in {
    AbsString.alpha("") shouldBe a [AbsStringProd]
  }
  it should behave like nonPreservingConcat
}

// Base class for string test.
// String tests need a lock because setting the string domain or string set size is a global side effect
//
//abstract class StringSet1Test extends StringTestBase {
//  override def beforeAll() {
//    Shell.params.opt_MaxStrSetSize = 1
//    super.beforeAll()
//  }
//
//  "SAFE default string" should behave like saneString
//  it should "hold one concrete string" in {
//    val s1 = AbsString.alpha("foo")
//    val s2 = AbsString.alpha("bar")
//    s1.gamma.value should contain only "foo"
//    (s1 + s2).gamma shouldBe None
//  }
//  it should behave like concatenation
//}
//
//abstract class StringSet2Test extends StringTestBase {
//  override def beforeAll() {
//    Shell.params.opt_MaxStrSetSize = 2
//    super.beforeAll()
//  }
//
//  "SAFE default string" should behave like saneString
//  it should behave like multiCaseStringSet
//  it should behave like concatenation
//}
