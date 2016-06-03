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
    o = o.update(strsAlpha("foo", "bar"), PropValue(toValue(42))) // update with abstract string set
    assertTrue(o.dom("foo"))  // foo is a possible property key
    assertEquals(BoolTop, o.domIn("foo"))  // but may be absent (thus BoolTop)
    assertTrue(o.dom("bar"))  // bar is a possible property key
    assertEquals(BoolTop, o.domIn("bar"))  // but may be absent (thus BoolTop)
    assertTrue(o.domIn("no such key") eq BoolFalse)  // key does definitely not exist
    o = o.update(StrTop, PropValue(toValue("white horse")))  // update with string top
    assertTrue(o.domIn("no such key") eq BoolTop)  // now any key may exist
    assertTrue(absEqHelper(o("no such key")._2, toValue("white horse")))  // its value is still concrete
    val size = o.size
    o = o.update(StrTop, PropValue(toValue("red horse")))  // string value no longer concrete
    assertEquals(size, o.size)  // size unchanged
    o = o.update("baz", PropValue(toValue(42)))  // strong update with concrete string
    assertEquals(size + 1, o.size)  // object map size increased by 1
    o = o.update("bar", PropValue(toValue(42)))  // strong update with concrete string
    assertEquals(BoolTrue, o.domIn("bar"))  // after the strong update, 'bar' can no longer be absent
  }

  @Ignore def testStringConcat = {
    Shell.params.opt_MaxStrSetSize = 2
    val a = toValue("foo")
    val b = Value(strsAlpha("bar", "baz"))
    val result = Operator.bopPlus(a,b)
    println(DomainPrinter.printValue(result))

    var o = Obj.empty
    o = o.update(strsAlpha("foo", "bar"), PropValue(toValue(42))) // update with abstract string set
    println(DomainPrinter.printObj(2, o))
  }
}

