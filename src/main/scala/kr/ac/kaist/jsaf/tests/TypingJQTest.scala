/*******************************************************************************
    Copyright (c) 2013, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ***************************************************************************** */

package kr.ac.kaist.jsaf.tests

import kr.ac.kaist.jsaf.{Shell, ShellParameters}
import kr.ac.kaist.jsaf.compiler.Predefined
import junit.framework.{Test, TestSuite}
import kr.ac.kaist.jsaf.analysis.typing.Config

class TypingJQTest

object TypingJQTest {
  val TESTS_DIR = "tests/typing_tests/jquery"

  val EXCLUDE = Set(
    "XXX",
    "NYI"
  )

  def main(args: String*) = junit.textui.TestRunner.run(suite)

  def suite(): Test = {
    val suite = new TestSuite("jQuery Semantics Test")
    val testcases = collectTestcase(TESTS_DIR)
    for (tc <- testcases) {
      //$JUnit-BEGIN$
      val jqtest = new SemanticsDOMTest(TESTS_DIR, tc, "dense")
      jqtest.configureFunc = () => {
        // legacy shell param
        Shell.params.Set(Array[String]("html", "-context-1-callsite", "-test", "-jq"))
        // setup testing options
        Config.setTestMode(true)
        Config.setAssertMode(true)
        // enable DOM
        Config.setDomMode
        // enable jQuery
        Config.setJQueryMode
      }
      suite.addTest(jqtest)
      //$JUnit-END$
    }
    suite
  }

  private def collectTestcase(dirname: String) = {
    val dir = FileTests.directoryAsFile(dirname)
    val filtered = dir.list.toSeq.filter(fname =>
      fname.endsWith(".html") &&
        !EXCLUDE.exists(prefix => fname.startsWith(prefix)))
    filtered.sorted
  }
}
