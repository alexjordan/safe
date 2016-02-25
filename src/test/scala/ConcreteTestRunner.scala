import java.io.File

import junit.framework.{TestSuite, Test}
import junit.framework.TestCase
import kr.ac.kaist.jsaf.compiler.Predefined
import kr.ac.kaist.jsaf.tests.ConcreteECMASemantics
import kr.ac.kaist.jsaf.{Shell, ShellParameters}


class ConcreteTestSuite extends TestCase("Concrete") {}

object ConcreteTestSuite {
  // TODO only one directory has tests so far
  val TESTS_DIR = "tests/concrete_semantics/ecma/GlobalObject"

  Shell.pred = new Predefined(new ShellParameters())

  def suite: Test = {
    val suite = new TestSuite("ConcreteSuite")

    // TODO simplistic filter for .js files
    for (filename <- Option(new File(TESTS_DIR).list).getOrElse(Array[String]()).filter(s => s.endsWith(".js")))
      suite.addTest(new ConcreteECMASemantics(TESTS_DIR, filename))
    suite
  }
}
