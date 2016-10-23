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
import junit.framework.TestCase
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.PreConfig
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._

import scala.collection.immutable.{HashSet => IHashSet}

class ProdTest extends TestCase("ProdTest") {

  type PR = AbsStringProd

  PreConfig.strings = TestStringConfig(StrDomainConst ++ StrDomainSet ++ StrDomainNumOth)
  
  def alpha(str: String): AbsStringProd =
    AbsStringProd.alpha(str)

  // Simple test.
  def test01 = {
    println("*** Test 01 ***")
    AbsStringSet.maxSize = 3
    var pr1 = (alpha("34") + alpha("123456790abc")).asInstanceOf[PR]
    var pr2 = alpha("128")
    assertFalse (pr2.contains(alpha("6")).getSingle.get)
    assertFalse (pr1.contains(alpha("8")).getSingle.get)
    assertTrue  (pr2.contains(alpha("1")).getSingle.get)
    assertTrue  ((pr1 <> pr2).isBottom)
    assertTrue  (pr1.isConcrete)
    assertTrue  (pr2.length.getSingle.get == 3)
    assertTrue  (new PR().isTop)
    assertTrue  (pr1 </ pr2 && pr2 </ pr1 && pr1 != pr2)
    assertTrue  (pr2.isAllNums && !pr1.isAllNums)
    assertFalse  ((pr1 + pr2).isTop)
    assertFalse (pr1 <= pr2 || pr2 <= pr1)
    assertFalse (pr1.isAllOthers || pr2.isAllOthers)
    assertTrue  (pr1.concat(pr2).isConcrete)
    assertTrue  (pr2.concat(pr2).getSingle.get == "128128")

    pr1 = alpha("p")
    pr2 = alpha("q")
    assertFalse  ((pr1 + pr2).isTop)
    assertEquals ((pr1 + pr2).toString, "{\"q\", \"p\"}")
    println("***   OK!   ***\n")
  }

  // Simulates the analysis of the following snippet of code:
  //   1. query = "SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE "
  //   2. if (l != null)
  //   3.   query += "WHOLESALE > " + l + " AND "
  //   4. per = "SELECT TYPESSDE, TYPEDESC FROM 
  //             TYPES WHERE NAME = 'fish' OR NAME = 'meat'"
  //   5. query += "TYPE IN ("  + per + ");
  def test02 = {
    println("*** Test 02 ***")
    var query = StrTop
    println("0. query => " + query)
    assertTrue(query.isTop)
    query = alpha("SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE ")
    println("1. query => " + query)
    val l = new PR()
    println("2. l => " + l)
    val query_if = 
      query.concat(alpha("WHOLESALE > ")).concat(l).concat(alpha(" AND "))
    val query_el = query
    query = query_if + query_el
    println("3. if: query => " + query_if)
    println(" else: query => " + query_el)
    println(" join: query => " + query)
    val per = alpha(
      "SELECT TYPESSDE, TYPEDESC FROM TYPES" +
      "WHERE NAME = 'fish' OR NAME = 'meat'"
    )
    println("4. per => " + per)
    query = query.concat(alpha("TYPE IN (")).concat(per).concat(alpha(");"))
    println("5. query => " + query)
    assertTrue(query.isTop)
    assertTrue(per.isAllOthers)
    assertTrue(per.isConcrete)
    println("***   OK!   ***\n")
  }

  // Simulates the analysis of the following snippet of code:
  //   1. string x = "a";
  //   2. while (cond)
  //   3.   x = "0" + x + "1"
  //   4. return x
  def test03 = {
    println("*** Test 03 ***")
    AbsStringSet.maxSize = 5
    var x = StrTop
    println("0. x => " + x)
    assertTrue(x.isTop)
    x = alpha("a")
    println("1. x => " + x)
    assertEquals(x.toString, "\"a\"")
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
        assertEquals(i, AbsStringSet.maxSize + 1)
        i = 0
      }
      else
        x_1 = (x_1 + x_2)
    } while (i > 0)
    x = ((x_1 <> cond) + x_2)
    println("4. x => " + x)
    assertTrue(x.isTop)
    assertEquals(x.toString, "String")

    println("***   OK!   ***\n")
  }

  def test04: Unit = {
    println("*** Test 04 ***")
    var x = NumStr
    println("0. x => " + x)
    assertTrue(x.isAllNums)
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
      x_2 = alpha("0").concat(x_2)
      println("  3. x => " + x_2)
      if (x_1.equals(x_1 + x_2)) {
        println("--- Fixpoint reached! x => " + x_1)
        assertEquals(i, 2)
        i = 0
      }
      else
        x_1 = x_1 + x_2
    } while (i > 0)
    x = x_1 <> cond + x_2
    println("4. x => " + x)
    assertTrue(x.isTop)
    println("***   OK!   ***\n")
  }

}

