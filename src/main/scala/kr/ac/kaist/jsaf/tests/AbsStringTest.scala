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

package kr.ac.kaist.jsaf.tests

import junit.framework.{Test, TestSuite}
import junit.framework.Assert._
import junit.framework.TestCase
import kr.ac.kaist.jsaf.analysis.typing.{AddressManager, Config, Operator, PreConfig}

import scala.collection.immutable.{HashSet => IHashSet}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._

class AbsStringTest extends TestCase("AbsStringTest") {

  // TODO the tests below don't use AbsString.alpha
  PreConfig.strings = TestStringConfig(StrDomainSAFE)

  def toValueL(in: List[Any]): Value = {
    var v: Value = ValueBot
    for (i <- in) {
      v = i match {
        case u: AbsUndef if u.isTop => v + Value(AbsUndef.alpha)
        case n: AbsNumber => n.getAbsCase match {
          case AbsSingle if !(n.getSingle.isDefined && AbsNumber.isNum(n)) => v + Value(n)
          case _ => v
        }
        case n: Int => v + Value(AbsNumber.alpha(n))
        case d: Number => v + Value(AbsNumber.alpha(d.doubleValue))
        case s: String => v + Value(AbsString.alpha(s))
        case b: Boolean => v + Value(AbsBool.alpha(b))
        case n: AbsNull if n.isTop => v + Value(AbsNull.alpha)
      }
    }
    v
  }

  def strAlpha(s: String): AbsString = AbsString.alpha(s)

  def strsAlpha(strs: String*): AbsString = 
    AbsStringSAFE.alpha(IHashSet(strs: _*))

  def toValue(in: Any) =
    toValueL(List(in))

  override def setUp(): Unit = {
    AddressManager.reset()
  }

  def testConcat = {
    assertEquals(AbsStringSAFE.alpha("foo") concat AbsStringSAFE.alpha("bar"), strAlpha("foobar"))
    AbsStringSet.maxSize = 2
    assertEquals(strsAlpha("foo", "bar") concat AbsStringSAFE.alpha("baz"), strsAlpha("foobaz", "barbaz"))
    AbsStringSet.maxSize = 1
    assertEquals(strsAlpha("foo", "bar") concat AbsStringSAFE.alpha("baz"), StrTop)
    assertTrue(OtherStr <= (strsAlpha("foo", "bar") concat AbsStringSAFE.alpha("123")))
    assertEquals(strAlpha("foo") concat StrTop, StrTop)
    // concatenation with a bottom element wipes out the string component of a primitive
    assertEquals(strAlpha("foo") concat StrBot, StrBot)
  }

  def testOrder = {
    assertFalse(toValue("") <= (toValue("foo") + Value(AbsUndef.UndefTop)))
    assertTrue((toValue("") + Value(AbsUndef.UndefTop)) <= (Value(StrTop) + Value(AbsUndef.UndefTop)))
    assertTrue(Value(OtherStr) <= (Value(StrTop) + Value(AbsUndef.UndefTop)))
  }

  def testJoin = {
    def alpha(str: String): AbsString =
      new AbsStringProd(AbsStringProd.ProdCase(Array(
        AbsStringSet.alpha(str), AbsStringNumOth.alpha(str)
      )))

    var x: AbsString = alpha("0")
    var y: AbsString = x concat alpha("1")
    var z: AbsString = x + y
    println(x, y, z)
    assertFalse(z.isAllNums)
    assertEquals(z.toString, "String")

    x = AbsStringSAFE.alpha("0")
    y = x concat AbsStringSAFE.alpha("1")
    z = x + y
    println(x, y, z)
    assertTrue(z.isAllNums)
    assertEquals(z.toString, "NumStr")

    def alpha2(str: String): AbsString =
      new AbsStringProd(AbsStringProd.ProdCase(Array(
        AbsStringSet.alpha(str), AbsStringNumOth.alpha(str), AbsStringSAFE.alpha(str)
      )))

    x = alpha2("0")
    y = x concat alpha2("1")
    z = x + y
    println(x, y, z)
    assertTrue(z.isAllNums)
    assertEquals(z.toString, "(String, String, NumStr)")
  }


  def testNum = {
    assertTrue((AbsStringSAFE.alpha("") concat strsAlpha("-1", "3.2")).isAllNums)
    assertEquals((strsAlpha("-1", "3.2") concat AbsStringSAFE.alpha("")).toString, "NumStr")
    assertTrue((AbsStringSAFE.alpha("-1") concat AbsNumber.alpha(2).toAbsString).isAllNums)
    assertTrue(AbsStringSAFE.alpha("1") != AbsStringSAFE.alpha("2"))
  }

  def testTrim = {
    val a1 = AbsStringSAFE.alpha("12345")
    val a2 = AbsStringSAFE.alpha("  12345\n\n").trim
    assertEquals(a1, a2)
    assertTrue(a2 <= a1 && a2 <= a1)
  }

  def test_toString: Unit = {
    assertEquals("NumStr", new AbsStringSAFE(AbsStringNumOth.NumStrCase).toString)
    assertEquals("OtherStr", new AbsStringSAFE(AbsStringNumOth.OthStrCase).toString)
  }

}

