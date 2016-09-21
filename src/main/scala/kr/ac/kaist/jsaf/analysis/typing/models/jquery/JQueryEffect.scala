/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
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

package kr.ac.kaist.jsaf.analysis.typing.models.jquery

import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing.{AccessHelper => AH, _}
import kr.ac.kaist.jsaf.analysis.typing.domain.Context
import kr.ac.kaist.jsaf.analysis.typing.models.AbsBuiltinFunc
import kr.ac.kaist.jsaf.analysis.typing.domain.Heap
import kr.ac.kaist.jsaf.analysis.cfg.{CFGExpr, CFG, FunctionId}

object JQueryEffect extends ModelData {
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("animate",     AbsBuiltinFunc("jQuery.prototype.animate", 4)),
    ("delay",       AbsBuiltinFunc("jQuery.prototype.delay", 2)),
    ("fadeIn",      AbsBuiltinFunc("jQuery.prototype.fadeIn", 3)),
    ("fadeOut",     AbsBuiltinFunc("jQuery.prototype.fadeOut", 3)),
    ("fadeTo",      AbsBuiltinFunc("jQuery.prototype.fadeTo", 4)),
    ("fadeToggle",  AbsBuiltinFunc("jQuery.prototype.fadeToggle", 3)),
    ("finish",      AbsBuiltinFunc("jQuery.prototype.finish", 1)),
    ("hide",        AbsBuiltinFunc("jQuery.prototype.hide", 3)),
    ("show",        AbsBuiltinFunc("jQuery.prototype.show", 3)),
    ("slideDown",   AbsBuiltinFunc("jQuery.prototype.slideDown", 3)),
    ("slideToggle", AbsBuiltinFunc("jQuery.prototype.slideToggle", 3)),
    ("slideUp",     AbsBuiltinFunc("jQuery.prototype.slideUp", 3)),
    ("stop",        AbsBuiltinFunc("jQuery.prototype.stop", 3))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (JQuery.ProtoLoc, prop_proto)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    List("animate", "delay", "fadeIn", "fadeOut", "fadeTo", "fadeToggle", "finish", "hide", "show",
      "slideDown", "slideToggle", "slideUp", "stop").foldLeft[Map[String, SemanticFun]](Map())((_m, name) =>
      _m + ("jQuery.prototype." + name -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          // do nothing
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          ((Helper.ReturnStore(h, Value(lset_this)), ctx), (he, ctxe))
        }))
    )
  }

}
