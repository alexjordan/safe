import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.tests.TestHelper._
import org.scalatest._
//import org.scalatest.Matchers._


class ObjTestSpec extends FlatSpec with Matchers {
  "Obj" should "allow map introspection" in {
    var o = Obj.empty
    o = o.update(AbsStringSet.alpha("foo"), PropValue(toValue(42)))
    val m = o.asMap
    m should be ('nonEmpty)
    m should contain key ("foo")
    m should not contain key ("bar")
  }

  it should "support concrete property lookup" in {
    var o = Obj.empty
    o = o.update(AbsStringSet.alpha("foo"), PropValue(toValue("foo_value")))
    o = o.update(AbsStringSet.alpha("bar"), PropValue(toValue(42)))
    o.concretePropAs[String]("foo") should be (Some("foo_value"))
    o.concretePropAs[Int]("bar") should be (Some(42))
  }
}
