package testhelpers

import kr.ac.kaist.jsaf.analysis.typing.domain.Value
import org.scalatest.matchers.{MatchResult, Matcher}

// TODO add more test matchers for abstract domains
trait AbstractMatchers {

  class AbstractEqualMatcher(right: Value) extends Matcher[Value] {

    def apply(left: Value) = {
      MatchResult(
        left <= right && right <= left,
        s"$left does not equal $right",
        s"$left equals $right"
      )
    }
  }

  def absEq(right: Value) = new AbstractEqualMatcher(right)
}

object AbstractMatchers extends AbstractMatchers
