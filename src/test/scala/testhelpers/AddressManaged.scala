package testhelpers

import kr.ac.kaist.jsaf.analysis.typing.AddressManager
import org.scalatest.{BeforeAndAfterAll, Suite}

trait AddressManaged extends BeforeAndAfterAll {
  self: Suite =>

  override def beforeAll(): Unit = {
    AddressManager.reset()
    super.beforeAll()
  }
}
