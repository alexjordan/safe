package kr.ac.kaist.jsaf.shellneo

import java.io.{BufferedWriter, File, FileWriter, IOException}

import kr.ac.kaist.jsaf.analysis.cfg.CFGBuilder
import org.rogach.scallop._
import kr.ac.kaist.jsaf.analysis.typing.{AddressManager, Config, InitHeap, Typing, domain}
import kr.ac.kaist.jsaf.{ProjectProperties, Shell}
import kr.ac.kaist.jsaf.exceptions.UserError
import kr.ac.kaist.jsaf.compiler.Parser
import kr.ac.kaist.jsaf.nodes.{IRRoot, Program}
import kr.ac.kaist.jsaf.nodes_util.{JSFromHTML, NodeUtil}
import kr.ac.kaist.jsaf.scala_src.nodes.{SProgram, STopLevel}
import edu.rice.cs.plt.tuple.{Option => JOption}
import kr.ac.kaist.jsaf.analysis.typing.domain.Obj
import kr.ac.kaist.jsaf.analysis.typing.models.DOMBuilder
import kr.ac.kaist.jsaf.tests.TestHelper
import kr.ac.kaist.jsaf.useful.{MemoryMeasurer, Pair, Useful}
import org.cyberneko.html.parsers.DOMParser
import org.w3c.dom.Document
import org.xml.sax.InputSource

import scala.collection.JavaConverters._

private class ShellConf(args: Seq[String]) extends ScallopConf(args) {
  val inputFiles = trailArg[List[String]](required = false, descr = ".js file(s) for analysis")
  val htmlFile = opt[String]("html", descr = "html file for analysis")
  val quiet = opt[Boolean]("quiet", descr = "less debug output")
  val domMode = opt[Boolean]("dom", descr = "dom mode")
  val exitDump = opt[Boolean]("exitdump")
  val cfgDump = opt[Boolean]("cfgdump")
  val heapVerbose = opt[Int]("heap-verbose", validate = (0 until 4) contains _ )
  val trace = opt[Boolean]("trace", descr = "trace output for AI semantics")
  val test = opt[Boolean]("test", descr = "expose abstract types for testing")
  val jquery = opt[Boolean]("jquery", descr = "enable jQuery model")
  val maxStrSet = opt[Int]("max-strset-size", descr = "max string set size", validate = _ > 0, default = Some(1))
  val timeout = opt[Int]("timeout", descr = "timeout in seconds", validate = _ > 0)
  requireOne(inputFiles, htmlFile)
}

object RunAnalysis {
  def main(args: Array[String]) = {
    val conf = new ShellConf(args)
    conf.verify()

    configure(conf)

    val (program, doc) =
      if (!conf.htmlFile.isSupplied)
        (parseJS(conf.inputFiles()), nullHTML)
      else
        parseHTML(conf.htmlFile())

    analyze(program, doc, conf)
  }

  val nullHTML = {
    val parser: DOMParser = new DOMParser
    parser.setFeature("http://xml.org/sax/features/namespaces", false)
    val str = new String("<html/>")
    parser.parse(new InputSource(new java.io.ByteArrayInputStream(str.getBytes())));
    parser.getDocument
  }

  def configure(conf: ShellConf) = {
    // Initialize AddressManager
    AddressManager.reset()

    val firstFile = if (conf.htmlFile.isDefined) conf.htmlFile() else conf.inputFiles().head
    Config.setFileName(firstFile)

    Config.setDefaultForinUnrollingCount(1)
    Config.setLoopSensitiveMode(true)
    Config.setContextSensitivityMode(Config.Context_Loop)
    Config.setContextSensitivityDepth(10)

    Shell.params.opt_MaxStrSetSize = conf.maxStrSet()

    // Initialize AbsString cache
    domain.AbsString.initCache

    if (conf.domMode() || conf.htmlFile.isSupplied) {
      System.out.println("DOM mode enabled.")
      Config.setDomMode
      Shell.params.opt_LocClone = true
      Config.setDOMPropMode
    }

    Shell.params.opt_ExitDump = conf.exitDump()
    if (conf.heapVerbose.isDefined)
      Config.setVerbose(conf.heapVerbose())
    Config.traceAI = conf.trace()
    Config.testMode = conf.test()
    Config.jqMode = conf.jquery()

    if (conf.timeout.isSupplied)
      Shell.params.opt_Timeout = conf.timeout()
  }

  def parseJS(files: Seq[String]) = {
    if (files.exists(fname => fname.endsWith(".html")))
      throw new UserError("html files not supported")
    Parser.fileToAST(files.asJava)
  }

  def parseHTML(fileName: String): (Program, Document) = {
    val jshtml = new JSFromHTML(fileName)
    (jshtml.parseScripts, jshtml.getDocument())
  }

  def analyze(_prog: Program, html: Document, conf: ShellConf) = {

    var program = _prog

    // concatenate modeled ASTs
    val SEP = File.separator
    val base = ProjectProperties.BASEDIR + SEP
    var modeledFiles: List[String] = List[String](base + "bin/models/builtin/__builtin__.js")
    var inputFiles: List[String] = List()
    if (Config.domMode) {
      modeledFiles :::= List(base + "bin/models/dom/__dom__.js")
      inputFiles :::= List(base + "bin/inputs/__input__.js")
    }

    Config.setModeledFiles(Config.getModeledFiles ++ modeledFiles ++ inputFiles)
    val modeledASTs: Program = Parser.fileToAST((modeledFiles ++ inputFiles).asJava)
    program = (modeledASTs, program) match {
      case (SProgram(info0, STopLevel(fds0, vds0, body0)), SProgram(info1, STopLevel(fds1, vds1, body1))) =>
        SProgram(info1, STopLevel(fds0 ++ fds1, vds0 ++ vds1, body0 ++ body1))
    }

    val irErrors = Shell.ASTtoIR(Config.fileName, program, JOption.none[String], JOption.none[kr.ac.kaist.jsaf.nodes_util.Coverage])
    val irOpt: JOption[IRRoot] = irErrors.first
    program = irErrors.third // Disambiguated and hoisted and with written

    // Check the translation result
    if (irOpt.isNone)
      throw new UserError("translation failed")
    val ir: IRRoot = irOpt.unwrap

    // Build CFG
    val builder = new CFGBuilder(ir)
    val cfg = builder.build
    val errors = builder.getErrors
    if (!(errors.isEmpty)) {
      Shell.reportErrors(NodeUtil.getFileName(ir), Shell.flattenErrors(errors), JOption.none[Pair[FileWriter, BufferedWriter]])
    }

    if (conf.cfgDump())
      cfg.dump()

    printf("# Initial peak memory(mb): %.2f\n", MemoryMeasurer.peakMemory)

    val init = new InitHeap(cfg)
    init.initialize

    // Set the initial state with DOM objects
    if (Config.domMode) {
      new DOMBuilder(cfg, init, html).initialize(false)
    }

    // Create Typing
    val typing = new Typing(cfg, conf.quiet(), Shell.params.opt_LocClone)
    Config.setTypingInterface(typing)

    // Check global variables in initial heap against list of predefined variables.
    init.checkPredefined

    // Analyze
    typing.analyze(init)

    // In quiet mode, the analysis does not print the iteration count, do it here
    if (conf.quiet()) {
      println("# Fixpoint iteration(#): " + typing.numIter)
    }

    printf("# Peak memory(mb): %.2f\n", MemoryMeasurer.peakMemory)
    printf("# Result heap memory(mb): %.2f\n", MemoryMeasurer.measureHeap)
    println("\n* Statistics *")
    println("# Total state count: " + typing.getStateCount)

    if (Config.testMode) {
      TestHelper.printTestObjects(typing)
    }
  }
}
