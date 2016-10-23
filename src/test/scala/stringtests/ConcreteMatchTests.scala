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
import org.junit.Assert._
import org.scalatest.OptionValues._
import testhelpers.{AddressManaged, Lockable}
import testhelpers.AbstractMatchers._

package ConcreteMatchTests {

  abstract class MyBase extends FunSuite with BeforeAndAfterAll {

    val concrete = 'isConcrete
    val bottom = 'isBottom
    val top = 'isTop
    val allNums = 'isAllNums
    val allOthers = 'isAllOthers

    def alpha(s: String): AbsString
    def stringSetup(): Unit

    override def beforeAll(): Unit = {
      Shell.params.opt_force_strdom = true // all tests are forced on their string domain
      stringSetup() // tests setup the string domain to be tested
      super.beforeAll() // propagate beforeAll setup
    }

    def testMatchingNO: Unit = {
      val oth = alpha("foo") + alpha("bar")
      assertTrue(oth.isAllOthers)
      val num = alpha("123") + alpha("456")
      assertTrue(num.isAllNums)
      val top = oth + num
      assertTrue(top.isTop)
      val bot = AbsStringNumOth.cast(StrBot)
      assertTrue(bot.isBottom)

      assertTrue(oth.matches("bar"))
      assertTrue(oth.matches(""))
      assertFalse(oth.matches("123"))
      assertTrue(top.matches("bar"))
      assertFalse(bot.matches("123"))
    }

    def testMatchingSS: Unit = {
      val s = alpha("foo")
      assertTrue(s.matches("foo"))
      assertFalse(s.matches("bar"))
      val t = s + alpha("bar")
      assert(t.matches("baz"))
      val top = AbsStringSet.cast(StrTop)
      assertTrue(top.matches(""))
      val bot = AbsStringSet.cast(StrBot)
      assertFalse(bot.matches(""))
    }
  }



  //abstract class HighLevelStringTest extends StringTestBase with AddressManaged

  class StringSSTest extends MyBase {
    override def stringSetup() {
      AbsStringSet.maxSize = 1
      PreConfig.strings = TestStringConfig(StrDomainSet)
    }

    override def alpha(s: String) = AbsStringSet.alpha(s)

    test("common tests") {
      testsFor(testMatchingSS)
    }
  }

  class StringNOTest extends MyBase {
    override def stringSetup() {
      AbsStringSet.maxSize = 1
      PreConfig.strings = TestStringConfig(StrDomainNumOth)
    }

    override def alpha(s: String) = AbsStringNumOth.alpha(s)

    test("common tests") {
      testsFor(testMatchingNO)
    }
  }

  class StringSAFETest extends MyBase {
    override def stringSetup() {
      AbsStringSet.maxSize = 1
      PreConfig.strings = TestStringConfig(StrDomainSAFE)
    }

    override def alpha(s: String) = AbsStringSAFE.alpha(s)

    test("assumptions") {
      val a = new AbsStringProd
      assert(a.isTop)
      val b = AbsStringProd.cast(StrBot)
      assert(b.isBottom)
    }
    test("common tests") {
      testsFor(testMatchingSS)
      testsFor(testMatchingNO)
    }

    test("specific tests") {
      val s = alpha("foo") + alpha("bar")
      assertFalse(s.isTop)
      val t = s + alpha("123")
      assertTrue(t.isTop)
    }
  }
}
