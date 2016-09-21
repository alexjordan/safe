/*******************************************************************************
    Copyright (c) 2013-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
  ******************************************************************************/
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

package kr.ac.kaist.jsaf.analysis.typing.models.DOMCore

import scala.collection.mutable.{Map=>MMap, HashMap=>MHashMap}
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml.{HTMLDocument, HTMLTopElement}
import org.w3c.dom.Node
import kr.ac.kaist.jsaf.analysis.cfg.{CFG, CFGExpr, InternalError, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing._
import scala.Some
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell

object DOMNode extends DOM {
  private val name = "Node"
  // NodeType
  val ELEMENT_NODE = 1
  val ATTRIBUTE_NODE = 2
  val TEXT_NODE = 3
  val CDATA_SECTION_NODE = 4
  val ENTITY_REFERENCE_NODE = 5
  val ENTITY_NODE = 6
  val PROCESSING_INSTRUCTION_NODE = 7
  val COMMENT_NODE = 8
  val DOCUMENT_NODE = 9
  val DOCUMENT_TYPE_NODE = 10
  val DOCUMENT_FRAGMENT_NODE = 11
  val NOTATION_NODE = 12

  /* predefined locatoins */
  val loc_cons = newSystemRecentLoc(name + "Cons")
  val loc_proto = newSystemRecentLoc(name + "Proto")
  val loc_ins = newSystemRecentLoc(name + "Ins")

  /* constructor or object*/
  private val prop_cons: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("@hasinstance", AbsConstValue(PropValueNullTop)),
    ("length", AbsConstValue(PropValue(ObjectValue(Value(AbsNumber.alpha(0)), F, F, F)))),
    ("prototype", AbsConstValue(PropValue(ObjectValue(Value(loc_proto), F, F, F)))),
    ("ELEMENT_NODE",                AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(1), F, T, T)))),
    ("ATTRIBUTE_NODE",              AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(2), F, T, T)))),
    ("TEXT_NODE",                   AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(3), F, T, T)))),
    ("CDATA_SECTION_NODE",          AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(4), F, T, T)))),
    ("ENTITY_REFERENCE_NODE",       AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(5), F, T, T)))),
    ("ENTITY_NODE",                 AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(6), F, T, T)))),
    ("PROCESSING_INSTRUCTION_NODE", AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(7), F, T, T)))),
    ("COMMENT_NODE",                AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(8), F, T, T)))),
    ("DOCUMENT_NODE",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(9), F, T, T)))),
    ("DOCUMENT_TYPE_NODE",          AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(10), F, T, T)))),
    ("DOCUMENT_FRAGMENT_NODE",      AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(11), F, T, T)))),
    ("NOTATION_NODE",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(12), F, T, T)))),
    ("DOCUMENT_POSITION_DISCONNECTED",            AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x01), F, T, T)))),
    ("DOCUMENT_POSITION_PRECEDING",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x02), F, T, T)))),
    ("DOCUMENT_POSITION_FOLLOWING",               AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x04), F, T, T)))),
    ("DOCUMENT_POSITION_CONTAINS",                AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x08), F, T, T)))),
    ("DOCUMENT_POSITION_CONTAINED_BY",            AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x10), F, T, T)))),
    ("DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC", AbsConstValue(PropValue(ObjectValue(AbsNumber.alpha(0x20), F, T, T))))
  )

  /* prorotype */
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("insertBefore",            AbsBuiltinFunc("DOMNode.insertBefore", 2)),
    ("replaceChild",            AbsBuiltinFunc("DOMNode.replaceChild", 2)),
    ("removeChild",             AbsBuiltinFunc("DOMNode.removeChild", 1)),
    ("appendChild",             AbsBuiltinFunc("DOMNode.appendChild", 1)),
    ("hasChildNodes",           AbsBuiltinFunc("DOMNode.hasChildNodes", 0)),
    ("cloneNode",               AbsBuiltinFunc("DOMNode.cloneNode", 1)),
    ("normalize",               AbsBuiltinFunc("DOMNode.normalize", 0)),
    ("isSupported",             AbsBuiltinFunc("DOMNode.isSupported", 2)),
    ("hasAttributes",           AbsBuiltinFunc("DOMNode.hasAttributes", 0)),
    ("compareDocumentPosition", AbsBuiltinFunc("DOMNode.compareDocumentPosition", 1)),
    ("isSameNode",              AbsBuiltinFunc("DOMNode.isSameNode", 1)),
    ("lookupPrefix",            AbsBuiltinFunc("DOMNode.lookupPrefix", 1)),
    ("isDefaultNamespace",      AbsBuiltinFunc("DOMNode.isDefaultNamespace", 1)),
    ("lookupNamespaceURI",      AbsBuiltinFunc("DOMNode.lookupNamespaceURI", 1)),
    ("isEqualNode",             AbsBuiltinFunc("DOMNode.isEqualNode", 1)),
    ("getFeature",              AbsBuiltinFunc("DOMNode.getFeature", 2)),
    ("setUserData",             AbsBuiltinFunc("DOMNode.setUserData", 3)),
    ("getUserData",             AbsBuiltinFunc("DOMNode.getUserData", 1)),
    // WHATWG DOM
    ("contains",                AbsBuiltinFunc("DOMNode.contains", 1))
  )

  /* prorotype */
  private val prop_proto2: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("insertBefore",            AbsBuiltinFunc("DOMNode.insertBefore", 2)),
    ("replaceChild",            AbsBuiltinFunc("DOMNode.replaceChild", 2)),
    ("removeChild",             AbsBuiltinFunc("DOMNode.removeChild", 1)),
    ("appendChild",             AbsBuiltinFunc("DOMNode.appendChild", 1)),
    ("hasChildNodes",           AbsBuiltinFunc("DOMNode.hasChildNodes", 0)),
    ("cloneNode",               AbsBuiltinFunc("DOMNode.cloneNode", 1)),
    ("normalize",               AbsBuiltinFunc("DOMNode.normalize", 0)),
    ("isSupported",             AbsBuiltinFunc("DOMNode.isSupported", 2)),
    ("hasAttributes",           AbsBuiltinFunc("DOMNode.hasAttributes", 0)),
    ("compareDocumentPosition", AbsBuiltinFunc("DOMNode.compareDocumentPosition", 1)),
    ("isSameNode",              AbsBuiltinFunc("DOMNode.isSameNode", 1)),
    ("lookupPrefix",            AbsBuiltinFunc("DOMNode.lookupPrefix", 1)),
    ("isDefaultNamespace",      AbsBuiltinFunc("DOMNode.isDefaultNamespace", 1)),
    ("lookupNamespaceURI",      AbsBuiltinFunc("DOMNode.lookupNamespaceURI", 1)),
    ("isEqualNode",             AbsBuiltinFunc("DOMNode.isEqualNode", 1)),
    ("getFeature",              AbsBuiltinFunc("DOMNode.getFeature", 2)),
    ("setUserData",             AbsBuiltinFunc("DOMNode.setUserData", 3)),
    ("getUserData",             AbsBuiltinFunc("DOMNode.getUserData", 1)),
    // WHATWG DOM
    ("contains",                AbsBuiltinFunc("DOMNode.contains", 1)),
    ("firstChild", AbsConstValue(PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set) + Value(NullTop), F, T, T)))),
    ("parentNode", AbsConstValue(PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set) + Value(NullTop), F, T, T)))),
    ("lastChild", AbsConstValue(PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set) + Value(NullTop), F, T, T)))),
    ("previousSibling", AbsConstValue(PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set) + Value(NullTop), F, T, T)))),
    ("nextSibling", AbsConstValue(PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set) + Value(NullTop), F, T, T))))
  )

 
  /* global */
  private val prop_global: List[(String, AbsProperty)] = List(
    (name, AbsConstValue(PropValue(ObjectValue(loc_cons, T, F, T))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = if(Shell.params.opt_Dommodel2) List(
    (loc_cons, prop_cons), (loc_proto, prop_proto2), (GlobalLoc, prop_global)
  ) else List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      //TODO: not yet implemented
      ("DOMNode.insertBefore" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(HTMLTopElement.loc_ins_set)), ctx), (he, ctxe))
          else {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val lset_new = getArgValue(h, ctx, args, "0").locs
          val ref = getArgValue(h, ctx, args, "1")
          val lset_ref = ref.locs
          // If refChild is null, insert newChild at the end of the list of children.
          val nullh = if(NullTop <= ref.pv._2) {
               DOMTree.appendChild(h, lset_this, lset_new)   
            } else h
          if (!lset_new.isEmpty && !lset_ref.isEmpty) {
            val h_1 = DOMTree.insertBefore(nullh, lset_this, lset_new, lset_ref)
            ((Helper.ReturnStore(h_1, Value(lset_new)), ctx), (he, ctxe))
          }
          else if(NullTop <= ref.pv._2)
            ((nullh, ctx), (he, ctxe))
          else 
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMNode.replaceChild" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(HTMLTopElement.loc_ins_set)), ctx), (he, ctxe))
          else {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val lset_new = getArgValue(h, ctx, args, "0").locs
          val lset_old = getArgValue(h, ctx, args, "1").locs
          if (!lset_new.isEmpty && !lset_old.isEmpty) {
            /* location for clone node */
            val h_1 = lset_this.foldLeft(h)((hh, l_node) => {
              val lset_ns = Helper.Proto(h, l_node, AbsString.alpha("childNodes")).locs
              lset_ns.foldLeft(hh)((hhh, l_ns) => {
                val n_len = Operator.ToUInt32(Helper.Proto(h, l_ns, AbsString.alpha("length")))
                n_len.getSingle match {
                  case Some(n) if AbsNumber.isNum(n_len) =>
                    val n_index = (0 until n.toInt).indexWhere((i) => {
                      BoolTrue <= Operator.bopSEq(Helper.Proto(hhh, l_ns, AbsString.alpha(i.toString)), Value(lset_old)).pv._3
                    })
                    if (n_index < 0)
                      hhh
                    else {
                      val hhh_1 = Helper.Delete(hhh, l_ns, AbsString.alpha(n_index.toString))._1
                      Helper.PropStore(hhh_1, l_ns, AbsString.alpha(n_index.toString), Value(lset_new))
                    }
                  case _ if AbsNumber.isUIntAll(n_len) =>
                    val b_eq = Operator.bopSEq(Helper.Proto(hhh, l_ns, NumStr), Value(lset_old)).pv._3
                    val hhh_1 =
                      if (BoolTrue <= b_eq) {
                        val _hhh = Helper.Delete(hhh, l_ns, NumStr)._1
                        Helper.PropStore(_hhh, l_ns, NumStr, Value(lset_new))
                      }
                      else HeapBot
                    val hhh_2 =
                      if (BoolFalse <= b_eq) hhh
                      else HeapBot
                    hhh_1 + hhh_2
                  case _ => hhh /* exception ?? */
                }
              })
            })
            /* `parentNode', 'previousSibling', 'nextSibling' update of the reference child */
            val (h_2, preSib, nextSib) = lset_old.foldLeft((h_1, ValueBot, ValueBot))((d, l) => {
              val preS = Helper.Proto(d._1, l, AbsString.alpha("previousSibling"))
              val nextS = Helper.Proto(d._1, l, AbsString.alpha("nextSibling"))
              val h_2_1 = Helper.PropStore(d._1, l, AbsString.alpha("parentNode"), Value(NullTop))
              val h_2_2 = Helper.PropStore(h_2_1, l, AbsString.alpha("previousSibling"), Value(NullTop))
              val h_2_3 = Helper.PropStore(h_2_2, l, AbsString.alpha("nextSibling"), Value(NullTop))
              (h_2_3, preS + d._2, nextS + d._3)
            })

            /* 'prarentNode', 'previousSibling', 'nextSibling' update of the new child */
            val h_3 = lset_new.foldLeft(h_2)((_h, l) => {
              val h_3_1 = Helper.PropStore(_h, l, AbsString.alpha("parentNode"), Value(lset_this))
              val h_3_2 = Helper.PropStore(h_3_1, l, AbsString.alpha("previousSibling"), preSib)
              Helper.PropStore(h_3_2, l, AbsString.alpha("nextSibling"), nextSib)
            })

            /* 'nextSibling' update of the previous sibling of the reference child */
            val h_4 = preSib.locs.foldLeft(h_3)((_h, l) =>
              Helper.PropStore(_h, l, AbsString.alpha("nextSibling"), Value(lset_new))
            )
            
            /* 'previousSibling' update of the next sibling of the reference child */
            val h_5 = nextSib.locs.foldLeft(h_4)((_h, l) =>
              Helper.PropStore(_h, l, AbsString.alpha("previousSibling"), Value(lset_new))
            )
            
            ((Helper.ReturnStore(h_5, Value(lset_old)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMNode.removeChild" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(HTMLTopElement.loc_ins_set)), ctx), (he, ctxe))
          else {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val lset_child = getArgValue(h, ctx, args, "0").locs
          if (!lset_this.isEmpty && !lset_child.isEmpty) {
            val h_1 = DOMTree.removeChild(h, lset_this, lset_child)
            ((Helper.ReturnStore(h_1, Value(lset_child)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMNode.appendChild" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val lset_child = getArgValue(h, ctx, args, "0").locs
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(lset_child)), ctx), (he, ctxe))
         else {
          val h_1 = DOMTree.appendChild(h, lset_this, lset_child)
          if (!lset_child.isEmpty && !lset_child.isEmpty)
            ((Helper.ReturnStore(h_1, Value(lset_child)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMNode.hasChildNodes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val b_return = lset_this.foldLeft[AbsBool](BoolBot)((b, l) => {
            val lset_child = Helper.Proto(h, l, AbsString.alpha("childNodes")).locs
            lset_child.foldLeft(b)((bb, ll) => {
              val absnum = Helper.Proto(h, ll, AbsString.alpha("length")).pv._4
              bb + (absnum.getAbsCase match {
                case AbsBot => BoolBot
                case _ if AbsNumber.isUIntAll(absnum) => BoolTop
                case _ => absnum.getSingle match {
                  case Some(n) if AbsNumber.isNum(absnum) => if (n != 0) BoolTrue else BoolFalse
                  case _ => BoolFalse
              }})
            })
          })
          if (b_return </ BoolBot)
            ((Helper.ReturnStore(h, Value(b_return)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMNode.cloneNode" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val b_deep = Helper.toBoolean(getArgValue(h, ctx, args, "0"))
          if (b_deep </ BoolBot) {
            if(Shell.params.opt_Dommodel2) 
              ((Helper.ReturnStore(h, Value(HTMLTopElement.loc_ins_set)), ctx), (he, ctxe))
            else {
            /* unsound, 'deep' arugment is ingnored */
            /* location for clone node */
            val l_r = addrToLoc(addr1, Recent)
            val (h_1, ctx_1)  = Helper.Oldify(h, ctx, addr1)
            /* this node only */
            val o_node = lset_this.foldLeft(Obj.bottom)((o, l) => o + h_1(l))
            val h_2 = h_1.update(l_r, o_node)
            /* The duplicate node has no parent; (parentNode is null.). */
            val h_3 = Helper.PropStore(h_2, l_r, AbsString.alpha("parentNode"), Value(NullTop))
            ((Helper.ReturnStore(h_3, Value(l_r)), ctx_1), (he, ctxe))
            }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.normalize" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* unsound, do nothing */
          ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
        })),
      ("DOMNode.isSupported" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_feature = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val s_version = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (s_feature </ StrBot || s_version </ StrBot)

          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.hasAttributes" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* imprecise semantic */
          ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
        })),
      ("DOMNode.compareDocumentPosition" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val v_other = getArgValue(h, ctx, args, "0")
          if (v_other </ ValueBot) {
            /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(UInt)), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.isSameNode" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val v_other = getArgValue(h, ctx, args, "0")
          if (v_other </ ValueBot) {
            val v_return = Operator.bopSEq(Value(lset_this), Value(v_other.locs))
            ((Helper.ReturnStore(h, v_return), ctx), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.lookupPrefix" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_uri = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (s_uri </ StrBot)
          /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(StrTop) + Value(NullTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.isDefaultNamespace" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_uri = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (s_uri </ StrBot)
          /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.lookupNamespaceURI" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_prefix = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (s_prefix </ StrBot)
          /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(StrTop) + Value(NullTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.isEqualNode" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val v_arg = getArgValue(h, ctx, args, "0")
          if (v_arg </ ValueBot)
          /* imprecise semantic */
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.getFeature" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_feature = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val s_version = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (s_feature </ StrBot || s_version </ StrBot)
          /* unsound semantic */
            ((Helper.ReturnStore(h, Value(NullTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.setUserData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_key = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val v_data = getArgValue(h, ctx, args, "1")
          val v_handler = getArgValue(h, ctx, args, "2")
          if (s_key </ StrBot || v_data </ ValueBot || v_handler </ ValueBot)
          /* unsound semantic */
            ((Helper.ReturnStore(h, Value(NullTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.getUserData" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* arguments */
          val s_key = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (s_key </ StrBot)
          /* unsound semantic */
            ((Helper.ReturnStore(h, Value(NullTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMNode.contains" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          if(Shell.params.opt_Dommodel2) 
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val other = getArgValue(h, ctx, args, "0") 
          val nullargcheck = if(other.pv._2 </ NullBot) Value(BoolFalse) else ValueBot

          val lset_other = getArgValue(h, ctx, args, "0").locs
          if(!lset_other.isEmpty){
            val returnval = lset_this.foldLeft(Value(BoolBot))((_val, l_this) => {
              lset_other.foldLeft(_val)((__val, l_other) => {
                if(DOMHelper.contains(h, LocSetBot, l_this, l_other) == true)
                  __val + Value(BoolTrue)
                else
                  __val + Value(BoolFalse)
              })
            })
            ((Helper.ReturnStore(h, returnval + nullargcheck), ctx), (he, ctxe))
          }
          else if(nullargcheck </ ValueBot)
            ((Helper.ReturnStore(h, nullargcheck), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        }))
    )
  }

  /* instance */
  override def getInstance(cfg: CFG): Option[Loc] = Some(newRecentLoc())
  
  /* list of properties in the instance object */
  override def getInsList(node: Node): List[(String, PropValue)] = {
    val nodeName = node.getNodeName
    val nodeValue = node.getNodeValue
    val namespaceURI = node.getNamespaceURI
    val prefix = node.getPrefix
    val localName = node.getLocalName
    val baseURI = node.getBaseURI
    val textContent = node.getTextContent
    List(
      // DOM Level 1
      ("nodeName",   PropValue(ObjectValue(AbsString.alpha(if(nodeName!=null) nodeName else ""), F, T, T))),
      ("nodeValue",   PropValue(ObjectValue(AbsString.alpha(if(nodeValue!=null) nodeValue else ""), T, T, T))),
      ("nodeType",   PropValue(ObjectValue(AbsNumber.alpha(node.getNodeType), F, T, T))),
      // Introduced in DOM Level 2
      ("namespaceURI",   PropValue(ObjectValue(AbsString.alpha(if(namespaceURI != null) namespaceURI else ""), F, T, T))),
      ("prefix",   PropValue(ObjectValue(AbsString.alpha(if(prefix!=null) prefix else ""), T, T, T))),
      ("localName",   PropValue(ObjectValue(AbsString.alpha(if(localName != null) localName else ""), F, T, T))),
      // Introduced in DOM Level 3
      //    ("baseURI",   PropValue(ObjectValue(AbsString.alpha(if(baseURI!=null) baseURI else ""), F, T, T))),
      ("textContent",   PropValue(ObjectValue(AbsString.alpha(if(textContent!=null) textContent else ""), T, T, T))),
      ("ownerDocument",   PropValue(ObjectValue(Value(HTMLDocument.GlobalDocumentLoc), F, T, T)))
      )
    // 'baseURI' in DOM Level 3
  }
  
  def getInsList2(): List[(String, AbsProperty)] = List(
    ("nodeName", AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, T)))),
    ("nodeValue", AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, T)))),
    ("nodeType", AbsConstValue(PropValue(ObjectValue(Value(NumTop), F, T, T)))),
    ("namespaceURI", AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, T)))),
    ("childNodes", AbsConstValue(PropValue(ObjectValue(Value(DOMNodeList.loc_ins2), F, T, T)))),
    ("attributes", AbsConstValue(PropValue(ObjectValue(Value(DOMNamedNodeMap.loc_ins2), F, T, T)))),
    ("ownerDocument", AbsConstValue(PropValue(ObjectValue(Value(HTMLDocument.loc_ins), F, T, T)))),
    ("prefix", AbsConstValue(PropValue(ObjectValue(Value(StrTop), T, T, T)))),
    ("localName", AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, T)))),
    ("textContext", AbsConstValue(PropValue(ObjectValue(Value(StrTop), T, T, T))))
  )

  def getInsList(node: Node, ownerDocument: PropValue): List[(String, PropValue)] = getInsList(node) :+
    ("ownerDocument", ownerDocument)

  def getInsList(nodeName: PropValue, nodeValue: PropValue, nodeType: PropValue, parentNode: PropValue, childNodes: PropValue,
                 firstChild: PropValue, lastChild: PropValue, previousSibling: PropValue, nextSibling: PropValue , ownerDocument: PropValue,
                 namespaceURI: PropValue, prefix: PropValue, localName: PropValue, textContent: PropValue) : List[(String, PropValue)] = List(
    ("nodeName", nodeName),
    ("nodeValue", nodeValue),
    ("nodeType", nodeType),
    ("parentNode", parentNode),
    ("childNodes", childNodes),
    ("firstChild", firstChild),
    ("lastChild", lastChild),
    ("previousSibling", previousSibling),
    ("nextSibling", nextSibling),
    ("ownerDocument", ownerDocument),
    ("namespaceURI", namespaceURI),
    ("prefix", prefix),
    ("localName", localName),
    ("textContent", textContent))
  // TODO: 'baseURI' in DOM Level 3
  


  def getInsList(nodeName: PropValue, nodeValue: PropValue, nodeType: PropValue, parentNode: PropValue, childNodes: PropValue,
                 firstChild: PropValue, lastChild: PropValue, previousSibling: PropValue, nextSibling: PropValue , ownerDocument: PropValue,
                 namespaceURI: PropValue, prefix: PropValue, localName: PropValue, textContent: PropValue, attributes: PropValue) : List[(String, PropValue)] = List(
    ("nodeName", nodeName),
    ("nodeValue", nodeValue),
    ("nodeType", nodeType),
    ("parentNode", parentNode),
    ("childNodes", childNodes),
    ("firstChild", firstChild),
    ("lastChild", lastChild),
    ("previousSibling", previousSibling),
    ("nextSibling", nextSibling),
    ("ownerDocument", ownerDocument),
    ("namespaceURI", namespaceURI),
    ("prefix", prefix),
    ("localName", localName),
    ("textContent", textContent),
    ("attributes", attributes)
  )
  // TODO: 'baseURI' in DOM Level 3


}
