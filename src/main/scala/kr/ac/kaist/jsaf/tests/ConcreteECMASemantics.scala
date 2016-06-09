package kr.ac.kaist.jsaf.tests

import java.io.File
import java.io.OutputStream
import java.io.PrintStream
import kr.ac.kaist.jsaf.Shell

import scala.collection.immutable.HashMap
import junit.framework.Assert.fail
import junit.framework.TestCase
import kr.ac.kaist.jsaf.analysis.cfg.CFG
import kr.ac.kaist.jsaf.analysis.cfg.CFGBuilder
import kr.ac.kaist.jsaf.analysis.cfg.LExit
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.compiler.Disambiguator
import kr.ac.kaist.jsaf.compiler.Hoister
import kr.ac.kaist.jsaf.compiler.Parser
import kr.ac.kaist.jsaf.compiler.Translator
import kr.ac.kaist.jsaf.compiler.WithRewriter
import kr.ac.kaist.jsaf.nodes.IRRoot
import kr.ac.kaist.jsaf.nodes.Program
import kr.ac.kaist.jsaf.nodes_util.NodeRelation
import kr.ac.kaist.jsaf.scala_src.useful.Options._

class ConcreteECMASemantics(testfile: File) extends TestCase(testfile.getName) {

  import ConcreteECMASemantics._

  private class NullOutputStream extends OutputStream {
    override def write(x: Int) = ()
  }

  override def runTest() = {
    // silence stdout, stderr
    val oldOut = System.out
    val oldErr = System.err
    val nullStream = new PrintStream(new NullOutputStream())
    System.setErr(nullStream)
    System.setOut(nullStream)

    try {
      // Initialize AddressManager
      AddressManager.reset()
      val typing = analyze(testfile)
      checkResult(typing)
    } finally {
      // recover stdout, stderr
      System.setErr(oldErr)
      System.setOut(oldOut)
    }
  }

  def analyze(file: File): TypingInterface = {
    // setup testing options
    Config.setTestMode(true)
    Config.setAssertMode(true)

    def findShell(f: File): Option[File] = {
      if (f == null || !f.exists())
        return None
      val list = f.listFiles()
      list.foreach(x => {
        if (x.getName equals "safe-shell.js")
          return Some(x)
      })
      findShell(f.getParentFile)
    }

    val shellFile: File = findShell(file.getParentFile) getOrElse {
      fail("No shell file found");
      new File("")
    }

    // parse
    var program: Program = Parser.filesToAST(Seq(shellFile.toString, file.toString))

    // hoist
    val hoister = new Hoister(program)
    program = hoister.doit().asInstanceOf[Program]

    // disambiguate
    val disambiguator = new Disambiguator(program, false)
    program = disambiguator.doit().asInstanceOf[Program]

    // with rewrite
    val withRewriter = new WithRewriter(program, false)
    program = withRewriter.doit().asInstanceOf[Program]

    // translate to IR
    val translator = new Translator(program, toJavaOption(None))
    val ir: IRRoot = translator.doit().asInstanceOf[IRRoot]

    // build CFG
    val builder = new CFGBuilder(ir)
    val cfg: CFG = builder.build()

    NodeRelation.set(program, ir, cfg, quiet = true)

    // initialize heap
    //val model = new BuiltinModel(cfg)
    //model.initialize()
    val init = new InitHeap(cfg)
    init.initialize()

    // typing
    val typing: TypingInterface = new Typing(cfg, locclone = false, quiet = false)

    // 10 callsite sensitive
    Config.setContextSensitivityMode(Config.Context_KCallsite)
    Config.setContextSensitivityDepth(10)


    Config.setTypingInterface(typing)
    Config.setAssertMode(true)
    //typing.analyze(init_heap)

    typing.analyze(init)

    // return resulting Typing instance
    typing
  }
}

object ConcreteECMASemantics {
  val RESULT = "__result"
  val EXPECT = "__expect"

  type CheckMap = Map[Int, Value]

  def parseResultExpect(obj: Obj): (CheckMap, CheckMap) = {
    // collect result/expect values
    var resultMap: CheckMap = HashMap()
    var expectMap: CheckMap = HashMap()

    for (prop <- obj.getProps) {
      try {
        val pvalue = obj(prop)
        if (prop.startsWith(RESULT)) {
          val index = prop.substring(RESULT.length).toInt
          resultMap += (index -> pvalue._1._1)
        } else if (prop.startsWith(EXPECT)) {
          val index = prop.substring(EXPECT.length).toInt
          expectMap += (index -> pvalue._1._1)
        }
      } catch {
        case _: Throwable => fail("Invalid result/expect variable found: " + prop.toString)
      }
    }
    (resultMap, expectMap)
  }

  def valueEq(v1: Value, v2: Value): Boolean = {
    def absEq(a1: AnyRef, a2: AnyRef): Boolean = {
      val aEq = (a1, a2) match {
        case (l: AbsUndef, r: AbsUndef) => l === r
        case (l: AbsNull, r: AbsNull) => l === r
        case (l: AbsBool, r: AbsBool) => l === r
        case (l: AbsNumber, r: AbsNumber) => l === r
        case (l: AbsString, r: AbsString) => l === r
        case _ => throw new AssertionError("incomparable")
      }
      if (aEq.isBottom)
        return true
      aEq.getSingle match {
        case Some(absBool) => absBool
        case _ => false
      }
    }

    val lazyStream: Stream[Boolean] = (v1.locset.isEmpty) #::
      (v2.locset.isEmpty) #::
      absEq(v1.pvalue._1, v2.pvalue._1) #::
      absEq(v1.pvalue._2, v2.pvalue._2) #::
      absEq(v1.pvalue._3, v2.pvalue._3) #::
      absEq(v1.pvalue._4, v2.pvalue._4) #::
      absEq(v1.pvalue._5, v2.pvalue._5) #::
      Stream.empty

    !lazyStream.contains(false)
  }

  def checkResult(typing: TypingInterface) = {
    // find global object at program exit node
    val state =
      typing.readTable(((typing.cfg.getGlobalFId, LExit), CallContext.globalCallContext))
    val heap = state._1

    //println(DomainPrinter.printHeap(2, heap, typing.cfg))

    var numMatches = 0
    for ((l, v) <- heap.map) {
      val obj: Obj = v
      if (obj.dom("type")) {
        val objmap = obj.asMap
        objmap("type")._1.objval._1._1._5.getSingle match {
          case Some("safe-testobj") =>
            //println(DomainPrinter.printObj(4, obj))
            val (resultMap, expectMap) = parseResultExpect(obj)
            for ((index, result) <- resultMap.toSeq.sortBy(_._1)) {
              expectMap.get(index) match {
                case Some(expect) =>
                  if (valueEq(expect, result)) {
                    numMatches += 1
                  } else {
                    println("result%d: %s".format(index, result))
                    println("expect%d: %s".format(index, expect))
                    fail("%s: result '%s' does not match expected '%s'".format(
                      objmap("description")._1.objval._1._1._5, result, expect))
                  }
                case None =>
                  fail("No corresponding expect variable is detected for " + RESULT + index.toString)
              }
            }
          case Some("safe-errorobj") =>
            fail("msg: " + objmap("reason")._1.objval._1._1._5)
          case _ =>
            fail("Broken test harness (type property missing)")
        }
      }
    }

    val globalObj = heap(GlobalLoc)
    val numTests: Int = globalObj("SAFE_TESTS")._2.pvalue.numval.getSingle.getOrElse(-1.0).toInt
    if (numTests != numMatches) {
      fail("Test results checked: %d, expected: %d".format(numTests, numMatches))
    }
  }
}
