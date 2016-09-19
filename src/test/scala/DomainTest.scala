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

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Assert._
import org.junit.Before
import org.junit.Test
import org.junit.Ignore
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.typing.{AddressManager, Helper, Operator, domain}

import scala.collection.immutable.{HashSet => IHashSet}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.tests.TestHelper._


class DomainTest extends AssertionsForJUnit() {

  @Before def setUp(): Unit = {
    AddressManager.reset()
  }

  @Ignore def testExperiment = {
    println(toValue("1") + toValue("2"))
    println(toValue("1") + toValue("x"))
    println(toValue(1) <= Value(domain.NumTop))
    println(toValue("foo") + toValue("foo"))
    println(Value(NumStr) + Value(OtherStr))
    println(AbsStringSet.alpha(IHashSet("foo", "bar")) + NumStr)
    println(StrBot.gamma)
    println(toValue("null1") + toValue("11") + toValue("null2") + toValue("12"))
    println(toValue("") + Value(AbsNull.NullTop))
    println(toValue("") + Value(AbsNull.NullBot))
    println(toValue("") + Value(AbsUndef.UndefTop))
  }

  @Ignore def testConcat = {
    assertEquals(AbsStringSet.alpha("foo") concat AbsString.alpha("bar"), strAlpha("foobar"))
    Shell.params.opt_MaxStrSetSize = 2
    assertEquals(strsAlpha("foo", "bar") concat AbsString.alpha("baz"), strsAlpha("foobaz", "barbaz"))
    Shell.params.opt_MaxStrSetSize = 1
    assertEquals(strsAlpha("foo", "bar") concat AbsString.alpha("baz"), OtherStr)
    assertTrue(OtherStr <= (strsAlpha("foo", "bar") concat AbsString.alpha("123")))
    assertEquals(strAlpha("foo") concat StrTop, StrTop)
    // concatenation with a bottom element wipes out the string component of a primitive
    assertEquals(strAlpha("foo") concat StrBot, StrBot)
  }

  @Test def testOrder = {
    assertFalse(toValue("") <= (toValue("foo") + Value(AbsUndef.UndefTop)))
    assertTrue((toValue("") + Value(AbsUndef.UndefTop)) <= (Value(AbsString.StrTop) + Value(AbsUndef.UndefTop)))
    assertTrue(Value(AbsStringSet.OtherStr) <= (Value(AbsString.StrTop) + Value(AbsUndef.UndefTop)))
  }

  // TODO scala 2.9 workaround
  def absEqHelper(lhs: AnyRef, rhs: AnyRef): Boolean =
    (lhs, rhs) match {
      case (v1: Value, v2: Value) => v1 <= v2 && v2 <= v1
    }

  @Test def testObjectStore = {
    var o = Obj.empty
    o = o.update(strsAlpha("foo", "bar"), makePropVal(toValue(42))) // update with abstract string set
    assertTrue(o.dom("foo"))  // foo is a possible property key
    assertEquals(BoolTop, o.domIn("foo"))  // but may be absent (thus BoolTop)
    assertTrue(o.dom("bar"))  // bar is a possible property key
    assertEquals(BoolTop, o.domIn("bar"))  // but may be absent (thus BoolTop)
    assertTrue(o.domIn("no such key") eq BoolFalse)  // key does definitely not exist
    o = o.update(StrTop, makePropVal(toValue("white horse")))  // update with string top
    assertTrue(o.domIn("no such key") eq BoolTop)  // now any key may exist
    assertTrue(absEqHelper(o("no such key")._2, toValue("white horse")))  // its value is still concrete
    val size = o.size
    o = o.update(StrTop, makePropVal(toValue("red horse")))  // string value no longer concrete
    assertEquals(size, o.size)  // size unchanged
    o = o.update("baz", makePropVal(toValue(42)))  // strong update with concrete string
    assertEquals(size + 1, o.size)  // object map size increased by 1
    o = o.update("bar", makePropVal(toValue(42)))  // strong update with concrete string
    assertEquals(BoolTrue, o.domIn("bar"))  // after the strong update, 'bar' can no longer be absent
  }

  @Test def testObjectPrecedes = {
    var o = Obj.empty
    var p = Obj.empty
    assertTrue(o <= p)
    o = o.update("@class", makePropVal(toValue(StrTop)))
    assertFalse(o <= p)
    p = p.update(StrTop, makePropVal(toValue(StrTop)))
    println(DomainPrinter.printObj(0, o))
    println(DomainPrinter.printObj(2, p))
    // assertTrue(o <= p) // fails by design?
  }

  @Ignore def testStringConcat = {
    Shell.params.opt_MaxStrSetSize = 2
    val a = toValue("foo")
    val b = Value(strsAlpha("bar", "baz"))
    val result = Operator.bopPlus(a,b)
    println(DomainPrinter.printValue(result))

    var o = Obj.empty
    o = o.update(strsAlpha("foo", "bar"), makePropVal(toValue(42))) // update with abstract string set
    println(DomainPrinter.printObj(2, o))
  }
}

