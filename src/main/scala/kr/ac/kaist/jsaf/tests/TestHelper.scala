/*******************************************************************************
 Copyright (c) 2016, Oracle and/or its affiliates.
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.
 * Neither the name of KAIST, S-Core, Oracle nor the names of its contributors
   may be used to endorse or promote products derived from this software without
   specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 This distribution may include materials developed by third parties.
 ******************************************************************************/

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
        case s: AbsString => Value(s)
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
        println("    result%d: %s".format(index, DomainPrinter.printValue(result)))
        expectMap.get(index) match {
          case Some(expect) =>
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
