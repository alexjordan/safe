import java.io.File

import junit.framework.{TestSuite, Test}
import junit.framework.TestCase
import kr.ac.kaist.jsaf.compiler.Predefined
import kr.ac.kaist.jsaf.tests.ConcreteECMASemantics
import kr.ac.kaist.jsaf.{Shell, ShellParameters}


class ConcreteTestSuite extends TestCase("Concrete") {}

object ConcreteTestSuite {
  val TESTS_DIR = "tests/concrete_semantics/ecma"

  Shell.pred = new Predefined(new ShellParameters())

  def suite: Test = {
    val suite = new TestSuite("ConcreteSuite")

    def collect(path: File): List[File] = {
      var l = List[File]()
      // TODO could apply filter here
      Option(path.listFiles()) match {
        case Some(files) =>
          files.foreach(f =>
            if (f.isDirectory)
              l ++= collect(f)
            else
              l :+= f
          )
        case None => ()
      }
      l
    }


    // TODO filter for files is .js extension
    for (file <- collect(new File(TESTS_DIR)).filter(f => f.getName.endsWith(".js")))
      suite.addTest(new ConcreteECMASemantics(file))
    suite

  }
}
