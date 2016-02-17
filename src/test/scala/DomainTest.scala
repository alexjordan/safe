package kr.ac.kaist.jsaf.tests

import junit.framework.{TestSuite, Test}
import junit.framework.Assert._
import junit.framework.TestCase
import kr.ac.kaist.jsaf.Shell
import kr.ac.kaist.jsaf.analysis.typing.{AddressManager, Helper, domain}
import scala.collection.immutable.{HashSet => IHashSet}

import kr.ac.kaist.jsaf.analysis.typing.domain._


class DomainTest extends TestCase("DomainTest") {
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

  def strAlpha: (String) => AbsString = AbsStringSet.alpha

  def strsAlpha(strs: String*): AbsString = AbsStringSet.alpha(IHashSet(strs: _*))

  def toValue(in: Any) =
    toValueL(List(in))

  override def setUp(): Unit = {
    AddressManager.reset()
  }

  def testExperiment = {
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

  def testConcat = {
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

  def testOrder = {
    assertFalse(toValue("") <= (toValue("foo") + Value(AbsUndef.UndefTop)))
    assertTrue((toValue("") + Value(AbsUndef.UndefTop)) <= (Value(AbsString.StrTop) + Value(AbsUndef.UndefTop)))
    assertTrue(Value(AbsStringSet.OtherStr) <= (Value(AbsString.StrTop) + Value(AbsUndef.UndefTop)))
  }
}

