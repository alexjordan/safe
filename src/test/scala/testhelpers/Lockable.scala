package testhelpers

import org.scalatest.{BeforeAndAfterAll, Suite}

import scala.concurrent.Lock

object Lockable {
  val lock = new Lock()
}

trait Lockable extends BeforeAndAfterAll {
  self: Suite =>

  override def beforeAll() {
    Lockable.lock.acquire()
    super.beforeAll()
  }

  override def afterAll() {
    Lockable.lock.release()
    super.afterAll()
  }
}
