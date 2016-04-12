package kr.ac.kaist.jsaf.tests

import kr.ac.kaist.jsaf.analysis.typing.domain.{AbsNull, AbsStringSet, _}
import scala.collection.immutable.{HashSet => IHashSet}

object TestHelper {
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
}
