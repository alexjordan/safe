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

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.tests.TestHelper._
import org.scalatest._
//import org.scalatest.Matchers._


class ObjTestSpec extends FlatSpec with Matchers {
  "Obj" should "allow map introspection" in {
    var o = Obj.empty
    o = o.update(AbsStringSet.alpha("foo"), PropValue(toValue(42)))
    val m = o.asMap
    m should be ('nonEmpty)
    m should contain key ("foo")
    m should not contain key ("bar")
  }

  it should "support concrete property lookup" in {
    var o = Obj.empty
    o = o.update(AbsStringSet.alpha("foo"), PropValue(toValue("foo_value")))
    o = o.update(AbsStringSet.alpha("bar"), PropValue(toValue(42)))
    o.concretePropAs[String]("foo") should be (Some("foo_value"))
    o.concretePropAs[Int]("bar") should be (Some(42))
  }
}
