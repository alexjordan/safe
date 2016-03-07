/*******************************************************************************
  * Copyright (c) 2012-2014, S-Core, KAIST.
  * All rights reserved.
  **
  *Use is subject to license terms.
  **
  *This distribution may include materials developed by third parties.
 ***************************************************************************** */
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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.Config
import kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml.HTMLTopElement
import kr.ac.kaist.jsaf.analysis.cfg.{CFG, InternalError}
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._

object DomainPrinter {
  def printHeap(ind: Int, heap: Heap, cfg: CFG, verbose_lv: Int): String = {
    val printer = new DomainPrinter(verbose_lv)
    printer.indent(ind)
    printer.ppHeap(ind, heap, cfg)
    printer.toString
  }

  def printHeap(ind: Int, heap: Heap, cfg: CFG): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.indent(ind)
    printer.ppHeap(ind, heap, cfg)
    printer.toString
  }

  def printContext(ind: Int, ctx: Context): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.indent(ind)
    printer.ppContext(ind, ctx)
    printer.toString
  }
  
  def printValue(value: Value): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.ppValue(value)
    printer.toString
  }

  def printLoc(l: Loc): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.ppLoc(l)
    printer.toString
  }
  
  def printLocSet(lset: LocSet): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.ppLocSet(lset)
    printer.toString
  }

  def printObj(ind: Int, o: Obj): String = {
    val printer = new DomainPrinter(Config.verbose)
    printer.indent(ind)
    printer.ppObj(ind, o, Config.verbose >= 2)
    printer.toString
  }

  var dbgIteration = 0
  def shouldPrint(absString: AbsString): Boolean = absString match {
    case s: AbsStringSAFE => StrDomainDefault == StrDomainSAFE
    case _ => true
  }
  def gammaPrint(absString: AbsString, result: String): Unit = {
    if (shouldPrint(absString)) {
      //println(s"string-log $dbgIteration: $absString => $result")
      //println(s"string-log $dbgIteration: => $result")
    }
  }
  def precedesPrint(lhs: AbsString, rhs: AbsString, result: Boolean): Unit = {
    if (shouldPrint(lhs)) {
      //println(s"string-log $dbgIteration: $lhs <= $rhs => $result")
      //println(s"string-log $dbgIteration: => $result")
    }
  }
}

private class DomainPrinter(verbose_lv: Int) {
  val sb = new StringBuilder()
  
  def indent(n: Int): Unit = {
    for (i <- 0 to n-1) sb.append(" ")
  }

  def newline(ind: Int, first: Boolean): Boolean = {
    if (!first) {
      sb.append("\n")
      indent(ind)
    }
    false
  }
  
  def ppHeap(ind: Int, heap: Heap, cfg: CFG): Unit = {
    var first = true
    for ((loc, obj) <- heap.map.toSeq.sortBy(_._1)) {
      // for non-verbose mode, locations for built-in are skipped.
      // verbose level1 : user location & html location
      // verbose level2 : user location & predefined location
      // verbose level3 : print all
      if ( verbose_lv == 3
          || (locToAddr(loc) >= locToAddr(CollapsedLoc) && !cfg.isHtmlAddr(locToAddr(loc)))
          //|| (verbose_lv == 1 && cfg.isHtmlAddr(locToAddr(loc)))
          || (verbose_lv == 1 && (HTMLTopElement.getInsLoc(heap).contains(loc) || cfg.isHtmlAddr(locToAddr(loc))))
          || (verbose_lv == 2 && locToAddr(loc) < locToAddr(CollapsedLoc))) {
      /*}
      if (verbose_lv ==  || locToAddr(loc) >= locToAddr(CollapsedLoc)) { */
        first = newline(ind, first)
        val len = ppLoc(loc)
        sb.append(" -> ")
        if (verbose_lv >= 2 || locToAddr(loc) != locToAddr(GlobalLoc)) {
          ppObj(ind+len+4, obj, true)
        } else {
          ppObj(ind+len+4, obj, false)
        }
      }
    }
    
    if (first) sb.append("Bot")
  }

  def ppContext(ind: Int, ctx: Context): Unit = {
    sb.append("{")
//    ppLocSet(ctx._1);
    sb.append("} X {")
//    ppLocSet(ctx._2)
    if (Config.verbose >= 2) {
      sb.append("} X {")
      ppAddrSet(ctx._3)
      sb.append("} X ")
      if (ctx._4 == null) {
        sb.append("Top")
      } else {
        sb.append("{")
        ppAddrSet(ctx._4)
        sb.append("}")
      }
    } else {
      sb.append("}")
    }
  }
  
  def ppObj(ind: Int, obj: Obj, verbose: Boolean): Unit = {
    var first = true
    val map = obj.getAllProps
    for ((prop) <- map.toSeq.sortBy(f => f)) {
      val pv = obj(prop)
      val abs = obj.domIn(prop)
      val show = verbose match {
        case true => true
        case false => Config.testMode match {
          case true =>
            !Config.globalVerboseProp(prop) &&
            !Config.testModeProp.contains(prop)
          case false =>
            !Config.globalVerboseProp(prop)
        }
      }
      
      if (show) { 
        first = newline(ind, first)
        val len = ppProp(prop)
        val arrow =
          if (!(BoolFalse <= abs) || prop.take(1) == "@") "  -> "
          else "  @-> "
        sb.append(arrow)
        ppPropValue(ind+len+arrow.length, pv)
      }
    }
    if (first) sb.append("{ }")
  }
  
  def ppPropValue(ind: Int, pv: PropValue): Unit = {
    var first = true

    val ov = pv._1
    if (ov != ObjectValueBot) {
      first = newline(ind, first)
      (ov._2, ov._3, ov._4) match {
        case (BoolBot, BoolBot, BoolBot) => sb.append("[VAL] ")
        case _ =>
          sb.append("[")
          ppBool(ov._2)
          ppBool(ov._3)
          ppBool(ov._4)
          sb.append("] ")
      }
      ppValue(ov._1)
    }
    
    val fun = pv._3
    if (fun != FunSetBot) {
      first = newline(ind, first)
      sb.append("[FUN] ")
      ppFunSet(fun)
    }
    
    if (first) sb.append("Bot")
  }

  def ppValue(v: Value): Unit = {
    var first = true

    def appendPretty(sb: StringBuilder, str: String) = {
      val maxlen = 40
      if (str.indexOf('\n') != -1) {
        sb.append("{multi-line-string} ")
        sb.append(str.substring(0, math.min(maxlen, str.length())).replaceAll("[\n\r]+", """\\n"""))
        if (str.length() > maxlen)
          sb.append("""(..)"""")
      } else {
        sb.append(str)
      }
    }

    if (v._1 != PValueBot) {
      appendPretty(sb, v._1.toString)
      first = false
    }
    
    if (v._2 != LocSetBot) {
      if (!first) sb.append(", ")
      ppLocSet(v._2)
      first = false
    }
    
    if (first) sb.append("Bot")
  }
  
  def ppProp(prop: String): Int = {
    val str = prop
    sb.append(str)
    str.length
  }
  
  def ppLoc(loc: Loc): Int = {
    val name = locName(loc)
    val str = isRecentLoc(loc) match {
      case true => "#" + name
      case false => "##" + name
    }
    sb.append(str)
    str.length 
  }

  def ppAddrSet(set: AddrSet): Unit = {
    var first = true
    for (addr <- set.toSeq.sorted) {
      if (first) {
        first = false
      } else {
        sb.append(", ")
      }
      ppLoc(addrToLoc(addr, Recent))
    }
  }

  def ppLocSet(set: LocSet): Unit = {
    var first = true
    for (loc <- set.toSeq.sorted) {
      if (first) {
        first = false
      } else {
        sb.append(", ")
      }
      ppLoc(loc)
    }
  }
  
  def ppFunSet(set: FunSet): Unit = {
    var first = true
    sb.append("{")
    for (fid <- set.toSeq.sorted) {
      if (first) {
        first = false
      } else {
        sb.append(", ")
      }
      sb.append(fid.toString)
    }
    sb.append("}")
  }

  def ppBool(b: AbsBool): Unit = {
    val str = b.getPair match {
      case (AbsBot, _) => "B"
      case (AbsSingle, Some(b)) => if (b) "t" else "f"
      case (AbsTop, _) => "T"
      case _ => throw new InternalError("AbsBool does not have an abstract value for multiple values.") 
    }
    sb.append(str)
  }
    
  override def toString = sb.toString
}
