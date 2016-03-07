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

import junit.framework.Assert._
import junit.framework.TestCase
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.typing.PreConfig
import kr.ac.kaist.jsaf.analysis.typing.domain.{PropValue, _}
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._
import kr.ac.kaist.jsaf.tests.TestHelper._
import org.junit.Ignore

import scala.collection.immutable.{HashSet => IHashSet}

@Ignore
class ObjTest extends TestCase("ObjTest") {

  PreConfig.strings = TestStringConfig(StrDomainSAFE  ++ StrDomainCharIncl)
  Obj.checkPropsValid = true

  def test01() = {
    var o = Obj.empty
    o = o.update(AbsString.alpha("foo"), PropValue(toValue(42)))
    val m = o.asMap
    assertTrue(m.nonEmpty)
    assertTrue(m.contains("foo"))
    assertFalse(m.contains("bar"))

    o = o.update(AbsString.alpha("foo"), PropValue(toValue("foo_value")))
    o = o.update(AbsString.alpha("bar"), PropValue(toValue(42)))
    assertEquals(o.concretePropAs[String]("foo"), Some("foo_value"))
    assertEquals(o.concretePropAs[Int]("bar"), Some(42))
    assertTrue(o.domIn(AbsString.alpha("foo")).getSingle.get)
    assertFalse(o.domIn(OtherStr).isConcrete)
    assertFalse(o.domIn(NumStr).getSingle.get)
  }

  def test02() = {
    var o = Obj.empty
    o = o.update(
      AbsStringCharIncl.alpha(IHashSet("foo", "bar")), PropValue(toValue(44))
    )
    assertFalse(o.domIn(AbsStringCharIncl.alpha("zoo")).getSingle.get)
    assertFalse(o.domIn("zoo").getSingle.get)
    assertTrue(o.domIn(NumStr).isTop)

    o = Obj.empty
    o = o.update(
      AbsStringCharIncl.alpha(IHashSet("0", "x")), PropValue(toValue(123))
    )
    assertFalse(o.domIn(AbsStringCharIncl.alpha("c")).getSingle.get)
  }

  def test03() = {
    var o = Obj.empty
    o = o.update(
      new AbsStringCharIncl(IHashSet('1', '2')), PropValue(toValue(12))
    )
    o = o.update(
      new AbsStringCharIncl(IHashSet('3', '4')), PropValue(toValue(34))
    )
    assertFalse(o.domIn(AbsStringCharIncl.alpha("5")).getSingle.get)
    assertFalse(o.domIn("67").getSingle.get)
    assertFalse(o.domIn(OtherStr).getSingle.get)

    assertEquals(o.apply("121")._1._1._1.toString, "12")
    assertTrue(o.apply("1")._1._1._1.strval.isBottom)
    assertFalse(o.domIn("2").getSingle.get)
    assertEquals(o.apply("433")._1._1._1.toString, "34")
    assertTrue(o.apply("3")._1._1._1.strval.isBottom)
    assertFalse(o.domIn("4").getSingle.get)
  }

  def test04() = {
    var o = Obj.empty
    o = o.update(
      AbsStringPrefSuff.alpha(IHashSet("1", "2")), PropValue(toValue(12))
    )
    o = o.update(
      new AbsStringPrefSuff("3", "4"), PropValue(toValue(34))
    )
    assertTrue(o.domIn(AbsStringPrefSuff.alpha("q")).isTop)
    assertTrue(o.domIn(OtherStr).isTop)
    assertTrue(o.apply("1")._1._1._1.strval.isBottom)
    assertTrue(o.domIn("2").isTop)
    assertEquals(o.apply("3abc4")._1._1._1.toString, "UInt")
  }

  def test05() = {
    var o = Obj.empty
    o = o.update(
      AbsStringNumOth.alpha(IHashSet("1", "2")), PropValue(toValue(12))
    )
    o = o.update(
      AbsStringNumOth.alpha(IHashSet("3", "4")), PropValue(toValue(34))
    )
    assertTrue(o.domIn(AbsStringNumOth.alpha("5")).isTop)
    assertFalse(o.domIn(AbsStringNumOth.alpha("x")).getSingle.get)
    assertFalse(o.domIn(OtherStr).getSingle.get)
    assertTrue(o.apply("1")._1._1._1.strval.isBottom)
    assertTrue(o.domIn("2").isTop)
    assertEquals(o.apply("58")._1._1._1.toString, "UInt")
  }

  def test06() = {
    Shell.params.opt_MaxStrSetSize = 2
    var o = Obj.empty
    o = o.update(
      new AbsStringSet(IHashSet("1", "2")), PropValue(toValue(12))
    )
    o = o.update(
      new AbsStringSet(IHashSet("3", "4")), PropValue(toValue(34))
    )
    assertFalse(o.domIn(AbsStringSet.alpha("5")).getSingle.get)
    assertFalse(o.domIn("x").getSingle.get)
    assertFalse(o.domIn(OtherStr).getSingle.get)
    assertEquals(o.apply("1")._1._1._1.toString, "12")
    assertTrue(o.domIn("2").isTop)
    assertTrue(o.apply("58")._1._1._1.numval.isBottom)
  }

  def test07() = {
    Shell.params.opt_MaxStrSetSize = 1
    val a =  Array(new AbsStringCharIncl(IHashSet('1', '2')), NumStr)
    val b =  Array(new AbsStringCharIncl(IHashSet('3', '4')), NumStr)
    var o = Obj.empty
    o = o.update(new AbsStringProd(a), PropValue(toValue(12)))
    o = o.update(new AbsStringProd(b), PropValue(toValue(34)))

    var c = Array(AbsStringCharIncl.alpha(IHashSet("5")), NumStr)
    assertFalse(o.domIn(new AbsStringProd(c)).getSingle.get)
    c =  Array(AbsStringCharIncl.alpha(IHashSet("67")), NumStr)
    assertFalse(o.domIn(new AbsStringProd(c)).getSingle.get)
    assertFalse(o.domIn(OtherStr).getSingle.get)
    c = Array(AbsStringCharIncl.alpha(IHashSet("121")), NumStr)
    assertEquals(o.apply(new AbsStringProd(c))._1._1._1.toString, "12")
    c = Array(AbsStringCharIncl.alpha(IHashSet("121")), NumStr)
    assertTrue(o.apply(new AbsStringProd(c))._1._1._1.strval.isBottom)
    c = Array(AbsStringCharIncl.alpha(IHashSet("2")), NumStr)
    assertFalse(o.domIn(new AbsStringProd(c)).getSingle.get)
    c = Array(AbsStringCharIncl.alpha(IHashSet("433")), NumStr)
    assertEquals(o.apply(new AbsStringProd(c))._1._1._1.toString, "34")
    c = Array(AbsStringCharIncl.alpha(IHashSet("3")), NumStr)
    assertTrue(o.apply(new AbsStringProd(c))._1._1._1.strval.isBottom)
    c = Array(AbsStringCharIncl.alpha(IHashSet("4")), NumStr)
    assertFalse(o.domIn(new AbsStringProd(c)).getSingle.get)
  }

  def test08() = {
    var o = Obj.empty
    val s1 = AbsString.alpha("a")
    val s2 = s1 + AbsString.alpha("aa")
    o = o.update(s1, PropValue(toValue(41)))  // concrete key update
    o = o.update(s2, PropValue(toValue(42)))  // abstract key update

    // concrete lookup: "aaa"
    val v1 = o("aaa")
    assertTrue(PropValue(toValue(42)) <= v1)
    assertTrue(PropValue(toValue(41)) </ v1)

    // concrete lookup: "a"
    val v2 = o("a"/*s1*/)
    assertTrue(PropValue(toValue(42)) <= v2)
    assertTrue(PropValue(toValue(41)) <= v2)

    // abstract lookup: (OtherStr, <Set(a)>)
    val v3 = o(s2)
    assertTrue(PropValue(toValue(42)) <= v3)
    assertTrue(PropValue(toValue(41)) <= v3)

  }

}

