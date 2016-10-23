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

import junit.framework.{Test, TestSuite}
import junit.framework.Assert._
import junit.framework.TestCase
import kr.ac.kaist.jsaf.Shell

import scala.collection.immutable.{HashSet => IHashSet}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.PreConfig
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig.StrDomainPrefSuff

class PrefSuffTest extends TestCase("PrefSuffTest") {

  PreConfig.strings = TestStringConfig(StrDomainPrefSuff)
  Shell.params.opt_force_strdom = true

  type PS = AbsStringPrefSuff
  
  def alpha(str: String): AbsStringPrefSuff = 
    AbsStringPrefSuff.alpha(str)

  def prefix(c: AbsString): String = c.asInstanceOf[PS].prefix

  def suffix(c: AbsString): String = c.asInstanceOf[PS].suffix

  // Simple test.
  def test01 = {
    println("*** Test 01 ***")
    var ps1 = new PS("128", "128")
    var ps2 = new PS("1283483948230", "3456128")
    assertFalse  (ps1.contains(alpha("1")).getSingle.isDefined)
    assertEquals (ps1.contains(alpha("7")), BoolTop)
    assertEquals (ps1 <> ps2, ps2)
    assertEquals (ps1 +  ps2, ps1)
    assertTrue   (alpha("").length == UInt)
    assertTrue   (new PS().isTop)
    assertTrue   (ps1 </ ps2 && ps2 <= ps1)
    assertFalse  (ps1.isAllNums   || ps2.isAllNums)
    assertFalse  (ps1.isAllOthers || ps2.isAllOthers)
    assertEquals (ps1.concat(ps2), new PS("128", "3456128"))
    assertEquals (ps2.concat(ps1), new PS("1283483948230", "128"))
    ps1 = new PS("p", "p")
    ps2 = new PS("q", "q")
    println(ps1.concat(ps2))
    assertTrue   (ps1 </ ps2 && ps2 </ ps1)
    assertEquals (ps1.concat(ps2).toString, "\"p\"..\"q\"")
    assertTrue   ((ps2 <> ps1).isBottom)
    println("***   OK!   ***\n")
  }
  

  // Simulates the analysis of the following snippet of code:
  //   1. query = "SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE "
  //   2. if (l != null)
  //   3.   query += "WHOLESALE > " + l + " AND "
  //   4. per = "SELECT TYPECODE, TYPEDESC FROM 
  //             TYPES WHERE NAME = 'fish' OR NAME = 'meat'"
  //   5. query += "TYPE IN ("  + per + ");
  def test02 = {
    println("*** Test 02 ***")
    var query = StrTop
    println("0. query => " + query)
    assertTrue(query.isTop)
    query = alpha("SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE ")
    println("1. query => " + query)
    val l = new PS()
    println("2. l => " + l)
    val query_if = 
      query.concat(alpha("WHOLESALE > ")).concat(l).concat(alpha(" AND "))
    val query_el = query
    query = query_if + query_el
    println("3. if: query => " + query_if)
    println(" else: query => " + query_el)
    println(" join: query => " + query)
    val per = alpha(
      "SELECT TYPECODE, TYPEDESC FROM TYPES" +
      "WHERE NAME = 'fish' OR NAME = 'meat'"
    )
    println("4. per => " + per)
    query = query.concat(alpha("TYPE IN (")).concat(per).concat(alpha(");"))
    println("5. query => " + query)
    val s1 = "SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE "
    val s2 = ");"
    assertEquals(prefix(query), s1)
    assertEquals(suffix(query), s2)
    println("***   OK!   ***\n")
  }

  // Simulates the analysis of the following snippet of code:
  //   1. string x = "a";
  //   2. while (cond)
  //   3.   x = "0" + x + "1"
  //   4. return x
  def test03 = {
    println("*** Test 03 ***")
    var x = StrTop
    println("0. x => " + x)
    assertTrue(x.isTop)
    x = alpha("a")
    println("1. x => " + x)
    assertEquals(x.toString, "\"a\"..\"a\"")
    var x_1 = x
    var x_2 = StrBot
    val cond = StrTop
    var i = 0
    do {
      i += 1
      println("While iteration no. " + i)
      var x_2 = x_1 <> cond 
      println("  2. x => " + x_2)
      x_2 = alpha("0").concat(x_2).concat(alpha("1"))
      println("  3. x => " + x_2)
      if (x_1.equals(x_1 + x_2)) {
        println("--- Fixpoint reached! x => " + x_1)
        i = 0
      }
      else
        x_1 = (x_1 + x_2)
    } while (i > 0)
    x = ((x_1 <> cond) + x_2)
    println("4. x => " + x)
    assertTrue(x.isTop)
    assertEquals(prefix(x), "")
    assertEquals(suffix(x), "")
    println("***   OK!   ***\n")
  }

  def test04: Unit = {
    println("*** Test 04 ***")
    var x = NumStr
    println("0. x => " + x)
    assertTrue(x.isTop)
    println("1. x => " + x)
    var x_1 = x
    var x_2 = StrBot
    val cond = StrTop
    var i = 0
    do {
      i += 1
      println("While iteration no. " + i)
      var x_2 = x_1 <> cond
      println("  2. x => " + x_2)
      x_2 = alpha("0").concat(x_2).asInstanceOf[PS]
      println("  3. x => " + x_2)
      if (x_1.equals(x_1 + x_2)) {
        println("--- Fixpoint reached! x => " + x_1)
        i = 0
      }
      else
        x_1 = (x_1 + x_2).asInstanceOf[PS]
    } while (i > 0)
    x = (x_1 <> cond + x_2)
    println("4. x => " + x)
    assertTrue(x.isTop)
    println("***   OK!   ***\n")
  }

}

