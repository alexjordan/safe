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

import junit.framework.TestCase
import junit.framework.Assert.assertEquals
import scala.sys.process._
import scala.collection.immutable.{HashSet => IHashSet}
import org.apache.commons.lang3.StringEscapeUtils.escapeJava

class StringTest extends TestCase("StringTest") {
  val TESTS_DIR = "tests/string_tests/"
  val OUTVAR = "OUTVAR"

  def toSet(str: String) : IHashSet[Char] = {
    var s = IHashSet[Char]()
    str.foreach { s += _ }
    s
  }

  def analyze(js: String, dom: String, out_val: String) = {

    println("*** Analysing " + js + " with strdom = " + dom)
    val cmd = "bin/neosafe --strdom=" + dom + " --exitdump " + TESTS_DIR + js
    val grp = "grep " + OUTVAR
    val out = ( cmd #| grp ).!!
    println(out)
    assertEquals(out.trim, OUTVAR + "  -> " + out_val)
    println("*** " + js + " successfully analysed ***\n")
  }

  def test_0a1() = {
    val js = "0a1.js"
    analyze(js, "ps,sf,no,co,ss,ns",
      "[ttf] (String, String, String, String, String, NotSplStr)")
    analyze(js, "ci,co,js", "[ttf] (<Set(a), Set(a, 1, 0)>, String, NotSplStr)")
    analyze(
      js, "ci,ps,co", "[ttf] (<Set(a), Set(a, 1, 0)>, String, String)"
    )
    analyze(js, "ps,co,no,sf,ss", "[ttf] String")
    analyze(js, "hy", "[ttf] (String, <Set(a), Set(a, 1, 0)>, String)")
  }

  def test_select() = {
    val js = "select.js"
    analyze(js, "co,ss,no,sf,ns", "[ttt] String")
    val lb = toSet(
      "SELECTTYPECODE,TYPEDESCFROMTYPESWHERENAME='fish'ORNAME='meat'()|IV10$/ ;"
    )
    val ub = lb union toSet("0123456789xXabcdefABCDEFInfinityNaN+->")
    val out_val_ci = "<" + lb.toString() + ", " + ub.toString() +  ">"
    analyze(js, "ci,co", "[ttt] (" + out_val_ci + ", String)")
    val out_val_ps = "\"SELECT '$$$' || (RETAIL/100) FROM INVENTORY WHERE \"" +
      "..\");\""
    analyze(js, "ps,co", "[ttt] (" + out_val_ps + ", String)")
    analyze(js, "co,no,sf,ss,js", "[ttt] String")
    analyze(js, "ci,ps,co",
            "[ttt] (" + out_val_ci + ", " + out_val_ps + ", String)")
    analyze(js, "hy", "[ttt] (String, " + out_val_ci + ", String)")
  }

  def test_padding() = {
    val js = "padding.js"
    analyze(js, "no,sf,ss,co,ns,js",
      "[ttf] (String, String, String, String, NotSplStr, NotSplStr)")
    val s = toSet("0123456789xXabcdefABCDEFInfinityNaN+-")
    val out_val = "<Set(), " + s.toString() +  ">"
    analyze(js, "ci,co", "[ttf] (" + out_val + ", String)")
    analyze(js, "ps,sf,no,co,ss", "[ttf] String")
    analyze(js, "hy", "[ttf] (String, " + out_val + ", String)")
  }

  def test_canvas() = {
    val js = "canvas.js"
    analyze(js, "no,co,ns,js", "[ttf] String")
    val s =
      "<canvas id=\"renderCanvas\" width=\"30px\" height=\"30px\"></canvas><scr"
    val lb = toSet(s + "]")
    val ub = lb union toSet(",0123456789xXabcdefABCDEFInfinityNaN+-")
    val out_val_ci =
      "<" + escapeJava(lb.toString()) + ", " + escapeJava(ub.toString()) +  ">"
    analyze(js, "no,sf,ss,co", "[ttf] String")
    analyze(js, "ci,co", "[ttf] {multi-line-string} " +
            "(<Set(e, s, x, \\n, n, ], =, t, <, a, i,  (..)\"")
    analyze(js, "ps,co", "[ttf] (\"" + escapeJava(s) + "\"..\"]\", String)")
    analyze(js, "hy", "[ttf] {multi-line-string} (String, <Set(e, s, x, \\n, n, ], =, t, <,(..)\"")
  }

  def test_lookup() = {
    val js = "lookup.js"
    val out_val = ", " +
      "#Object.prototype.propertyIsEnumerable, #Object.prototype.isPrototypeOf, "+
      "#Object.prototype.hasOwnProperty, #Object.prototype.valueOf, " +
      "#Object.prototype.toLocaleString, #Object.prototype.toString, #ObjectConst"
    analyze(js, "sf", "[ttf] undefined, \"foo\"" + out_val)
    analyze(js, "ns,sf", "[ttf] undefined, \"foo\"")
    analyze(js, "ps,co", "[ttf] undefined, \"foo\"")
    analyze(js, "ci,co", "[ttf] undefined, \"foo\"")
    analyze(js, "js",    "[ttf] undefined, \"foo\"")
    analyze(js, "hy",    "[ttf] undefined, \"foo\"")
  }

}