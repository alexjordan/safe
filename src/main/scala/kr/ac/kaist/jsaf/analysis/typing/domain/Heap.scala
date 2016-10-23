/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
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

package kr.ac.kaist.jsaf.analysis.typing.domain

import kr.ac.kaist.jsaf.analysis.typing.Config
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

case class Heap(map: HeapMap) {
  /* partial order */
  def <= (that: Heap): Boolean = {
    this.map.submapOf(that.map)
  }
  
  /* join */
  def + (that: Heap): Heap = {
    if (this.map eq that.map) this
    else if (this eq HeapBot) that
    else if (that eq HeapBot) this
    else {
      val this_map = this.map
      val that_map = that.map
      val join_map = 
        if (this_map.size < that_map.size) {
          this_map.foldLeft(that_map)((m, kv) => m.weakUpdated(kv._1, kv._2))
        } else {
          that_map.foldLeft(this_map)((m, kv) => m.weakUpdated(kv._1, kv._2))
        }
      Heap(join_map)
    }
  }

  /* meet */
  def <> (that: Heap): Heap = {
    if (this.map eq that.map) this
    else if (this.map.isEmpty) HeapBot
    else if (that.map.isEmpty) HeapBot
    else {
      val meet = that.map.foldLeft(this.map)(
        (m, kv) => m.get(kv._1) match {
          case None => m - kv._1
          case Some(v) => m + (kv._1 -> (kv._2 <> v))
        })
      Heap(meet)
    }
  }

  /* lookup */
  def apply(loc: Loc): Obj = map.get(loc) match {
    case Some(obj) => obj
    case None => Obj.bottom
  }

  /* heap update */
  def update(loc: Loc, obj: Obj): Heap = {
    // A heap holding bottom object represents no valid concrete heap.
    // For pre-analysis, input heap is unchanged when attempting such update.
    // For main analysis, HeapBot is returned, indicating dead state.
    if (Config.preAnalysis) {
      if (obj.isBottom) this
      else Heap(map.weakUpdated(loc, obj))
    } 
    else {
      // recent location
      if ((loc & 1) == 0) {
        if (obj.isBottom) HeapBot
        else Heap(map.updated(loc, obj))
      }
      // old location
      else { 
        if (obj.isBottom) {
          if (this(loc).isBottom) HeapBot
          else this
        }
        else Heap(map.weakUpdated(loc, obj))
      }
    }
  }

  /* remove location */
  def remove(loc: Loc): Heap = {
    Heap(map - loc)
  }

  /* substitute l_r by l_o */
  def subsLoc(l_r: Loc, l_o: Loc): Heap = {
    Heap(this.map.subsLoc(l_r, l_o))
  }

  def domIn(loc: Loc) = { map.contains(loc) }

  def restrict(lp: LPSet) = {
    val m = map.foldLeft(map)((m, kv) =>
      lp.get(kv._1) match {
        case None => m - kv._1
        case Some(s) => m + (kv._1 -> m(kv._1).restrict(s))
      })

    Heap(m)
  }

  def restrict(lset: LocSet) =
  posMask match {
    case Some(posmask) => Heap(this.map.filter((kv) => lset.contains(if (kv._1 < 0) (kv._1 | negMask.get) else (kv._1 & posmask))))
    case None => Heap(this.map.filter((kv) => lset.contains(kv._1)))
  }

  /* for temporal pre-analysis result, make all the properties absentTop. */
  def absentTop() = {
    Heap(this.map.map((kv) => (kv._1 -> (kv._2.absentTop()))))
  }

}

