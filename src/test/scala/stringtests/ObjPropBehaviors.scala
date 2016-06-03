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
      s._1._5.gamma.value should contain only "foobar"
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

