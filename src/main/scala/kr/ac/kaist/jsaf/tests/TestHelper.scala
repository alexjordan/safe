package kr.ac.kaist.jsaf.tests

import junit.framework.Assert._
import kr.ac.kaist.jsaf.analysis.cfg.LExit
import kr.ac.kaist.jsaf.analysis.typing.domain.{AbsNull, AbsStringSet, _}
import kr.ac.kaist.jsaf.analysis.typing.{CallContext, Helper, TypingInterface}

import scala.collection.immutable.HashMap
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

  val RESULT = "__result"
  val EXPECT = "__expect"
  type CheckMap = Map[Int, Value]

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

  def findTestObjects(typing: TypingInterface, typeString: String = "safe-shell"): Seq[Obj] = {
    val heap = Helper.getHeapAtExit(typing)

    var ret = Seq[Obj]()
    for ((l, v) <- heap.map) {
      val obj: Obj = v
      if (obj.dom("type")) {
        val objmap = obj.asMap
        if (objmap("type")._1.objval._1._1._5 <= AbsString.alpha(typeString)) {
          ret :+= obj
        }
      }
    }
    ret
  }

  def printTestObjects(typing: TypingInterface): Unit = {
    def printFunc(obj: Obj, header: String = "Test obj") = {
      val (resultMap, expectMap) = parseResultExpect(obj)
      if (resultMap.nonEmpty)
        println(s"- ${header}:")
      for ((index, result) <- resultMap.toSeq.sortBy(_._1)) {
        expectMap.get(index) match {
          case Some(expect) =>
            println("    result%d: %s".format(index, DomainPrinter.printValue(result)))
            println("    expect%d: %s".format(index, DomainPrinter.printValue(expect)))
          case None =>
            println("No corresponding expect variable is detected for " + RESULT + index.toString)
        }
      }
    }

    // print result/expected paris in global object
    printFunc(Helper.getHeapAtExit(typing)(GlobalLoc), "Global obj")
    // print result/expected paris in all test objects on the heap
    findTestObjects(typing).foreach((o:Obj) => printFunc(o))
  }
}
