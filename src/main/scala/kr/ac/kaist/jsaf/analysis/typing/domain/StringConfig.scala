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

import kr.ac.kaist.jsaf.Shell._
import kr.ac.kaist.jsaf.analysis.typing.domain.StringConfig._

object StringConfig {
  type StrDomainType = Seq[Int]
  val StrDomainConst:     StrDomainType = Seq(0)
  val StrDomainSet:       StrDomainType = Seq(1)
  val StrDomainNumOth:    StrDomainType = Seq(2)
  val StrDomainSAFE:      StrDomainType = Seq(3)
  val StrDomainCharIncl:  StrDomainType = Seq(4)
  val StrDomainPrefSuff:  StrDomainType = Seq(5)
  val StrDomainAutomata:  StrDomainType = Seq(6)
  val StrDomainNumOthSpl: StrDomainType = Seq(7)
  val StrDomainJSAI:      StrDomainType = Seq(8)
  val StrDomainHash:      StrDomainType = Seq(9)
}

trait StringConfig {
  def getStrDoms: StrDomainType
}

class CmdLineStringConfig(strDomParam: String) extends StringConfig {
  // TODO result of this could be cached
  def getStrDoms: StrDomainType = {
    var strdomtype = Seq[Int]()
    val doms = strDomParam.split(",")
    val n = doms.length
    assert(n >= 1)
    for (d <- doms) {
      d match {
        case "co"
          => strdomtype ++= StrDomainConst
        case "sf"
          => strdomtype ++= StrDomainSAFE
        case "ss"
          => strdomtype ++= StrDomainSet
        case "au"
          => strdomtype ++= StrDomainAutomata
        case "no"
          => strdomtype ++= StrDomainNumOth
        case "ns"
          => strdomtype ++= StrDomainNumOthSpl
        case "ci"
          => strdomtype ++= StrDomainCharIncl
        case "ps"
          => strdomtype ++= StrDomainPrefSuff
        case "js"
          => strdomtype ++= StrDomainJSAI
        case "sh"
          => strdomtype ++= StrDomainHash
        case "hy"
          => if (params.opt_MaxStrSetSize < 3)
               params.opt_MaxStrSetSize = 3
             return StrDomainSet ++ StrDomainCharIncl ++ StrDomainHash
      }
    }
    strdomtype
  }
}

class TestStringConfig(doms: StrDomainType) extends StringConfig {
  def getStrDoms = doms
}

object TestStringConfig {
  def apply(doms: StrDomainType) = new TestStringConfig(doms)
}

