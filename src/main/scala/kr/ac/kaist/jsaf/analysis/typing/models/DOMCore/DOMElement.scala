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
import org.w3c.dom.Node
import org.w3c.dom.Element
import kr.ac.kaist.jsaf.analysis.cfg.{CFG, CFGExpr, InternalError, FunctionId}
import kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml.{HTMLDocument, HTMLCollection, HTMLTopElement}
import kr.ac.kaist.jsaf.analysis.typing.models.DOMObject.{ClientRect, ClientRectList}
import kr.ac.kaist.jsaf.analysis.typing._
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell

object DOMElement extends DOM {
  private val name = "Element"

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
    ("prototype", AbsConstValue(PropValue(ObjectValue(Value(loc_proto), F, F, F))))
  )

  /* prorotype */
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(DOMNode.loc_proto), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("getAttribute",           AbsBuiltinFunc("DOMElement.getAttribute", 1)),
    ("setAttribute",           AbsBuiltinFunc("DOMElement.setAttribute", 2)),
    ("removeAttribute",        AbsBuiltinFunc("DOMElement.removeAttribute", 1)),
    ("getAttributeNode",       AbsBuiltinFunc("DOMElement.getAttributeNode", 1)),
    ("setAttributeNode",       AbsBuiltinFunc("DOMElement.setAttributeNode", 1)),
    ("removeAttributeNode",    AbsBuiltinFunc("DOMElement.removeAttributeNode", 1)),
    ("getElementsByTagName",   AbsBuiltinFunc("DOMElement.getElementsByTagName", 1)),
    ("getAttributeNS",         AbsBuiltinFunc("DOMElement.getAttributeNS", 2)),
    ("setAttributeNS",         AbsBuiltinFunc("DOMElement.setAttributeNS", 3)),
    ("removeAttributeNS",      AbsBuiltinFunc("DOMElement.removeAttributeNS", 2)),
    ("getAttributeNodeNS",     AbsBuiltinFunc("DOMElement.getAttributeNodeNS", 2)),
    ("setAttributeNodeNS",     AbsBuiltinFunc("DOMElement.setAttributeNodeNS", 1)),
    ("getElementsByTagNameNS", AbsBuiltinFunc("DOMElement.getElementsByTagNameNS", 2)),
    ("hasAttribute",           AbsBuiltinFunc("DOMElement.hasAttribute", 1)),
    ("hasAttributeNS",         AbsBuiltinFunc("DOMElement.hasAttributeNS", 2)),
    ("setIdAttribute",         AbsBuiltinFunc("DOMElement.setIdAttribute", 2)),
    ("setIdAttributeNS",       AbsBuiltinFunc("DOMElement.setIdAttributeNS", 3)),
    ("setIdAttributeNode",     AbsBuiltinFunc("DOMElement.setIdAttributeNode", 2)),
    ("querySelector",           AbsBuiltinFunc("DOMElement.querySelector", 0)),
    ("querySelectorAll",        AbsBuiltinFunc("DOMElement.querySelectorAll", 0)),
    // WHATWG DOM
    ("getElementsByClassName",      AbsBuiltinFunc("DOMElement.getElementsByClassName", 2)),
    // W3C CSSOM View Module
    ("getClientRects",      AbsBuiltinFunc("DOMElement.getClientRects", 0)),
    ("getBoundingClientRect",      AbsBuiltinFunc("DOMElement.getBoundingClientRect", 0)),
    ("scrollInfoView",      AbsBuiltinFunc("DOMElement.scrollIntoView", 1)),
    // Non-standard
    ("webkitMatchesSelector",           AbsBuiltinFunc("DOMElement.webkitMatchesSelector", 1))
  )

  /* global */
  private val prop_global: List[(String, AbsProperty)] = List(
    (name, AbsConstValue(PropValue(ObjectValue(loc_cons, T, F, T))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map(
      //TODO: not yet implemented
      ("DOMElement.getAttribute" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val attr_name = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0"))).toLowerCase
          if(Shell.params.opt_Dommodel2){
            if(attr_name </ StrBot){
              ((Helper.ReturnStore(h, Value(StrTop)), ctx), (he, ctxe))
            }
            else 
            ((HeapBot, ContextBot), (he, ctxe))
          }
          else {
          // get attribute
          val v_ret = DOMHelper.getAttribute(h, lset_this, attr_name)
          if(v_ret </ ValueBot)
            ((Helper.ReturnStore(h, v_ret), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
          }
        })),
      ("DOMElement.setAttribute" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val addr2 = cfg.getAPIAddress(addr_env, 1)
          val addr3 = cfg.getAPIAddress(addr_env, 2)
          val addr4 = cfg.getAPIAddress(addr_env, 3)
          val addr5 = cfg.getAPIAddress(addr_env, 4)
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          // For 'Attr' node
          val l_attr = addrToLoc(addr1, Recent)
          // For 'Text' node
          val l_text = addrToLoc(addr2, Recent)
          // For NamedNodeList for 'childNodes' of the Attr and text nodes
          val l_child1 = addrToLoc(addr3, Recent)
          val l_child2 = addrToLoc(addr4, Recent)
          // For classTable look-up entry
          val l_classentry = addrToLoc(addr5, Recent)
          val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
          val (h_2, ctx_2) = Helper.Oldify(h_1, ctx_1, addr2)
          val (h_3, ctx_3) = Helper.Oldify(h_2, ctx_2, addr3)
          val (h_4, ctx_4) = Helper.Oldify(h_3, ctx_3, addr4)
          val (h_5, ctx_5) = Helper.Oldify(h_4, ctx_4, addr5)
          /* arguments */
          val attr_name = Helper.toString(Helper.toPrimitive_better(h_5, getArgValue(h_5, ctx_5, args, "0"))).toLowerCase
          val attr_val = Helper.toString(Helper.toPrimitive_better(h_5, getArgValue(h_5, ctx_5, args, "1")))

          /* imprecise semantics : no exception handling */
          if(attr_name </ StrBot || attr_val </StrBot) {

            if(Shell.params.opt_Dommodel2){
              ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
            }
            else {
            val h_6 = DOMHelper.setAttribute(h_5, lset_this, l_attr, l_text, l_child1, l_child2, l_classentry, attr_name, attr_val)
            ((h_6, ctx_5), (he, ctxe))
            }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      ("DOMElement.removeAttribute" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val attr_name = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0"))).toLowerCase
          if(attr_name </ StrBot){
            if(Shell.params.opt_Dommodel2){
              ((Helper.ReturnStore(h, Value(UndefTop)), ctx), (he, ctxe))
            }
            else {
            // remove attribute
            val h1 = DOMHelper.removeAttribute(h, lset_this, attr_name)
            ((Helper.ReturnStore(h1, Value(UndefTop)), ctx), (he, ctxe))
            }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),

      //case "DOMElement.getAttributeNode" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setAttributeNode" => ((h, ctx), (he, ctxe))
      //case "DOMElement.removeAttributeNode" => ((h, ctx), (he, ctxe))
      ("DOMElement.getElementsByTagName" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
         /* imprecise modeling */
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1)  = Helper.Oldify(h, ctx, addr1)

          /* arguments */
          val tagname = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if (tagname </ StrBot) {
            if(Shell.params.opt_Dommodel2){
              val lset = Helper.Proto(h, TagTableLoc, tagname.toUpperCase).locs
              val proplist = HTMLCollection.getInsList(0) 
              val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
              val new_obj = if(lset.size > 0) 
                                    obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                                NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                            else obj
              val h_2 = h_1.update(l_r, new_obj)
              ((Helper.ReturnStore(h_2,  Value(l_r)), ctx_1), (he, ctxe))
            }
            else {
            (tagname.getSingle,  lset_this.size) match {
               case (Some(v), 1) =>
                 DOMTree.toConcreteDOMTree(h, lset_this.head, DOMTree.new_concrete_Document) match {

                   case Some((node, map)) =>
                     node match {
                       case e: Element =>
                          val nodelist = e.getElementsByTagName(v)
                          val len = nodelist.getLength
                          val proplist = HTMLCollection.getInsList(len) 
                          val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                          val newobj = (0 until len).foldLeft(obj)((o, i) => {
                              o.update(i.toInt.toString, PropValue(ObjectValue(Value(map(nodelist.item(i))), T, T, T)))
                            })
                          val h_2 = h_1.update(l_r, newobj)
                          ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
                       case _ => 
                         throw new InternalError("Error in DOMTree.toConcreteDOMTree")
                     }
                   case _ =>

                    val lset = lset_this.foldLeft(LocSetBot)((lset, l) => lset ++ DOMHelper.findByTag(h_1, l, tagname))
                    val proplist = HTMLCollection.getInsList(0) 
                    val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                    val newobj = if(lset.size==0) obj
                                 else if(lset.size==1) {
                                   obj.update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(1)), F, T, T))).update(
                                               "0", PropValue(ObjectValue(Value(lset), T, T, T)))
                                 }
                                 else {
                                   obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                               NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                                 }
                    val h_2 = h_1.update(l_r, newobj)
                    ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))

               }

               case _  =>

                val lset = lset_this.foldLeft(LocSetBot)((lset, l) => lset ++ DOMHelper.findByTag(h_1, l, tagname))
                val proplist = HTMLCollection.getInsList(0) 
                val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                val newobj = if(lset.size==0) obj
                             else if(lset.size==1) {
                               obj.update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(1)), F, T, T))).update(
                                           "0", PropValue(ObjectValue(Value(lset), T, T, T)))
                             }
                             else {
                               obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                           NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                             }
                val h_2 = h_1.update(l_r, newobj)
                ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
              }
              }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        })),
      //case "DOMElement.getAttributeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setAttributeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.removeAttributeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.getAttributeNodeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setAttributeNodeNS" => ((h, ctx), (he, ctxe))
      "DOMElement.getElementsByTagNameNS" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          /* arguments */
          val s_ns = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          val s_name = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "1")))
          if (s_ns </ StrBot && s_name </ StrBot) {
            val obj_table = h(TagTableLoc)
            val propv_element = obj_table(s_name.toUpperCase)
            val abs_element = obj_table.domIn(s_name.toUpperCase)
            val (h_1, ctx_1, v_empty) =
              if (BoolFalse <= abs_element) {
                val l_r = addrToLoc(addr1, Recent)
                val (_h, _ctx) = Helper.Oldify(h, ctx, addr1)
                /* empty NodeList */
                val o_empty = DOMNodeList.getInsList(0).foldLeft(Obj.empty)((o, pv) =>
                  o.update(pv._1, pv._2))
                val _h1 = _h.update(l_r, o_empty)
                (_h1, _ctx, Value(l_r))
              } else (h, ctx, ValueBot)
            /* imprecise semantic */
            ((Helper.ReturnStore(h_1, propv_element._1._1 + v_empty), ctx_1), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "DOMElement.hasAttribute" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val attr_name = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0"))).toLowerCase
          if(attr_name </ StrBot) {
            if(Shell.params.opt_Dommodel2){
              ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
            }
            else {
            val attr_val = lset_this.foldLeft(ValueBot)((v, l_this) => {
              // read the list of attributes in the current node
              val attributes_lset = Helper.Proto(h, l_this, AbsString.alpha("attributes")).locs
              val attr_val_1 = attributes_lset.foldLeft(ValueBot)((v, l_attributes) => {
                v + Value(Helper.HasOwnProperty(h, l_attributes, attr_name))
              })
              v + attr_val_1
            })
            ((Helper.ReturnStore(h, attr_val), ctx), (he, ctxe))
            }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      //case "DOMElement.hasAttributeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setIdAttribute" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setIdAttributeNS" => ((h, ctx), (he, ctxe))
      //case "DOMElement.setIdAttributeNode" => ((h, ctx), (he, ctxe))
      "DOMElement.querySelector" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)

          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val s_selector = getArgValue(h, ctx, args, "0").pv._5
          if (s_selector </ StrBot) {
            val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
            // start here
            val l_result = addrToLoc(addr1, Recent)
            val lset_find = lset_this.foldLeft(LocSetBot)((ls, l) => ls ++ DOMHelper.querySelectorAll(h_1, l, s_selector))
            val (h_ret, v_ret) =
              if (lset_find.isEmpty)
                (h_1, Value(NullTop))
              else {
                val o_result = Helper.NewObject(ObjProtoLoc)
                  .update("0", PropValue(ObjectValue(Value(lset_find), T, T, T)))
                  .update("length", PropValue(ObjectValue(AbsNumber.alpha(0), T, T, T)))
                val h_2 = h_1.update(l_result, o_result)
                (h_2, Value(lset_find))
              }
            ((Helper.ReturnStore(h_ret, v_ret), ctx_1), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "DOMElement.querySelectorAll" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)

          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          /* arguments */
          val s_selector = getArgValue(h, ctx, args, "0").pv._5
          if (s_selector </ StrBot) {
            val (h_1, ctx_1) = Helper.Oldify(h, ctx, addr1)
            val l_result = addrToLoc(addr1, Recent)
            if(Shell.params.opt_Dommodel2){
                val o_result = Helper.NewObject(ObjProtoLoc)
                  .update(NumStr, PropValue(ObjectValue(Value(HTMLTopElement.loc_ins_set), T, T, T)))
                  .update("length", PropValue(ObjectValue(Value(UInt), F, F, F)))
                val h_2 = h_1.update(l_result, o_result)
              ((Helper.ReturnStore(h_2, Value(l_result)), ctx), (he, ctxe))
            }
            // start here
            val lset_find = lset_this.foldLeft(LocSetBot)((ls, l) => ls ++ DOMHelper.querySelectorAll(h_1, l, s_selector))
            val (h_ret, v_ret) =
              if (lset_find.isEmpty) {
                val o_result = Helper.NewObject(ObjProtoLoc)
                  .update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(0)), T, T, T)))
                val h_2 = h_1.update(l_result, o_result)
                (h_2, Value(l_result))

              }
              else if(lset_find.size==1) {
                val o_result = Helper.NewObject(ObjProtoLoc)
                  .update("0", PropValue(ObjectValue(Value(lset_find), T, T, T)))
                  .update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(1)), F, F, F)))
                val h_2 = h_1.update(l_result, o_result)
                (h_2, Value(l_result))
              }

              else {
                val o_result = Helper.NewObject(ObjProtoLoc)
                  .update(NumStr, PropValue(ObjectValue(Value(lset_find), T, T, T)))
                  .update("length", PropValue(ObjectValue(Value(UInt), F, F, F)))
                val h_2 = h_1.update(l_result, o_result)
                (h_2, Value(l_result))
              }
            ((Helper.ReturnStore(h_ret, v_ret), ctx_1), (he, ctxe))
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      "DOMElement.getElementsByClassName" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          val lset_this = h(SinglePureLocalLoc)("@this")._2.locs
          val lset_env = h(SinglePureLocalLoc)("@env")._2.locs
          val set_addr = lset_env.foldLeft[Set[Address]](Set())((a, l) => a + locToAddr(l))
          if (set_addr.size > 1) throw new InternalError("API heap allocation: Size of env address is " + set_addr.size)
          val addr_env = (cp._1._1, set_addr.head)
          val addr1 = cfg.getAPIAddress(addr_env, 0)
          val l_r = addrToLoc(addr1, Recent)
          val (h_1, ctx_1)  = Helper.Oldify(h, ctx, addr1)
          /* arguments */
          val s_class = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))

          if (s_class </ StrBot) {
            if(Shell.params.opt_Dommodel2){
              val lset = Helper.Proto(h, ClassTableLoc, s_class).locs
              val proplist = HTMLCollection.getInsList(0) 
              val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
              val new_obj = if(lset.size > 0) 
                                    obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                                NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                            else obj
              val h_2 = h_1.update(l_r, new_obj)
              ((Helper.ReturnStore(h_2,  Value(l_r)), ctx_1), (he, ctxe))
            }
            else {
            (s_class.getSingle,  lset_this.size) match {
               case (Some(v), 1) =>
                 DOMTree.toConcreteDOMTree(h, lset_this.head, DOMTree.new_concrete_Document) match {
                   case Some((node, map)) =>
                     val nodelist = DOMTree.getElementsByClassName_concrete(node, v)
                     val len = nodelist.size
                     val proplist = HTMLCollection.getInsList(len) 
                     val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                     val newobj = (0 until len).foldLeft(obj)((o, i) => {
                        o.update(i.toInt.toString, PropValue(ObjectValue(Value(map(nodelist(i))), T, T, T)))
                       })
                     val h_2 = h_1.update(l_r, newobj)
                     ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
                   case _ =>
                     val lset = lset_this.foldLeft(LocSetBot)((lset, l) => lset ++ DOMHelper.findByClassName(h_1, l, s_class))
                     val proplist = HTMLCollection.getInsList(0) 
                     val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                     val newobj = if(lset.size==0) obj
                                  else if(lset.size==1) {
                                    obj.update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(1)), F, T, T))).update(
                                               "0", PropValue(ObjectValue(Value(lset), T, T, T)))
                                  }
                                  else {
                                    obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                                NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                                  }
                     val h_2 = h_1.update(l_r, newobj)
                     ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
               }

               case _  =>
                 val lset = lset_this.foldLeft(LocSetBot)((lset, l) => lset ++ DOMHelper.findByClassName(h_1, l, s_class))
                 val proplist = HTMLCollection.getInsList(0) 
                 val obj = proplist.foldLeft(Obj.empty)((o, p) => o.update(p._1, p._2))
                 val newobj = if(lset.size==0) obj
                              else if(lset.size==1) {
                                obj.update("length", PropValue(ObjectValue(Value(AbsNumber.alpha(1)), F, T, T))).update(
                                           "0", PropValue(ObjectValue(Value(lset), T, T, T)))
                              }
                              else {
                                obj.update("length", PropValue(ObjectValue(Value(UInt), F, T, T))).update(
                                            NumStr, PropValue(ObjectValue(Value(lset), T, T, T)))
                              }
                 val h_2 = h_1.update(l_r, newobj)
                 ((Helper.ReturnStore(h_2, Value(l_r)), ctx_1), (he, ctxe))
              }
              }
          }
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }),
      ("DOMElement.getClientRects" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
           ((Helper.ReturnStore(h, Value(ClientRectList.loc_ins)), ctx), (he, ctxe))
        })),

      ("DOMElement.getBoundingClientRect" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
           ((Helper.ReturnStore(h, Value(ClientRect.loc_ins)), ctx), (he, ctxe))
        })),

      // could be more precise
      ("DOMElement.webkitMatchesSelector" -> (
        (sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG, fun: String, args: CFGExpr) => {
          /* argument */
          val selector = Helper.toString(Helper.toPrimitive_better(h, getArgValue(h, ctx, args, "0")))
          if(selector </ StrBot)
            ((Helper.ReturnStore(h, Value(BoolTop)), ctx), (he, ctxe))
          else
            ((HeapBot, ContextBot), (he, ctxe))
        }))
    )
  }

  /* instance */
  override def getInstance(cfg: CFG): Option[Loc] = Some(newRecentLoc())
  /* list of properties in the instance object */
  override def getInsList(node: Node): List[(String, PropValue)] = node match {
    case e: Element =>
      // This instance object has all properties of the Node object
      DOMNode.getInsList(node) ++ List(
        // DOM Level 1
        ("tagName",   PropValue(ObjectValue(AbsString.alpha(e.getTagName), F, T, F))),
        // non-standard
        ("scrollTop", PropValue(ObjectValue(UInt, T, T, T))),
        ("scrollLeft", PropValue(ObjectValue(UInt, T, T, T))),
        ("scrollWidth", PropValue(ObjectValue(UInt, F, T, T))),
        ("scrollHeight", PropValue(ObjectValue(UInt, F, T, T))),
        ("offsetParent", PropValue(ObjectValue(NullTop, F, T, T))),
        ("offsetTop", PropValue(ObjectValue(UInt, F, T, T))),
        ("offsetLeft", PropValue(ObjectValue(UInt, F, T, T))),
        ("offsetWidth", PropValue(ObjectValue(UInt, F, T, T))),
        ("offsetHeight", PropValue(ObjectValue(UInt, F, T, T))),
        ("clientTop", PropValue(ObjectValue(UInt, F, T, T))),
        ("clientLeft", PropValue(ObjectValue(UInt, F, T, T))),
        ("clientWidth", PropValue(ObjectValue(UInt, F, T, T))),
        ("clientHeight", PropValue(ObjectValue(UInt, F, T, T))),
        ("onclick", PropValue(ObjectValue(NullTop, T, T, T))),
        ("onload", PropValue(ObjectValue(NullTop, T, T, T)))
        // 'style' property is updated in the DOMBuilder module
      )
    // TODO: schemaTypeInfo in DOM Level 3
    case _ => {
      System.err.println("* Warning: " + node.getNodeName + " cannot have instance objects.")
      List()
    }
  }

  def getInsList2():List[(String, AbsProperty)] = DOMNode.getInsList2() ++ List(
    ("tagName", AbsConstValue(PropValue(ObjectValue(Value(StrTop), F, T, F)))),
    ("scrollTop", AbsConstValue(PropValue(ObjectValue(Value(UInt), T, T, T)))),
    ("scrollLeft", AbsConstValue(PropValue(ObjectValue(Value(UInt), T, T, T)))),
    ("scrollWidth", AbsConstValue(PropValue(ObjectValue(Value(UInt), T, T, T)))),
    ("scrollHeight", AbsConstValue(PropValue(ObjectValue(Value(UInt), T, T, T)))),
    ("offsetTop", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("offsetLeft", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("offsetWidth", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("offsetHeight", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("clientTop", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("clientWidth", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T)))),
    ("offs", AbsConstValue(PropValue(ObjectValue(Value(UInt), F, T, T))))
  )

  def getInsList(tagName: PropValue, scrollTop: PropValue, scrollLeft: PropValue, scrollWidth: PropValue, scrollHeight: PropValue,
                 offsetParent: PropValue, offsetTop: PropValue, offsetLeft: PropValue, offsetWidth: PropValue, offsetHeight: PropValue,
                 clientTop: PropValue, clientLeft: PropValue, clientWidth: PropValue, clientHeight: PropValue, onclick: PropValue,
                 onload: PropValue): List[(String, PropValue)] = List(
    ("tagName", tagName),
    ("scrollTop", scrollTop),
    ("scrollLeft", scrollLeft),
    ("scrollWidth", scrollWidth),
    ("scrollHeight", scrollHeight),
    ("offsetParent", offsetParent),
    ("offsetTop", offsetTop),
    ("offsetLeft", offsetLeft),
    ("offsetWidth", offsetWidth),
    ("offsetHeight", offsetHeight),
    ("clientTop", clientTop),
    ("clientLeft", clientLeft),
    ("clientWidth", clientWidth),
    ("clientHeight", clientHeight),
    ("onclick", onclick),
    ("onload", onload)
  )

  def getInsList(tagName: PropValue): List[(String, PropValue)] = {
    val nodeName = tagName
    val nodeValue = PropValue(ObjectValue(NullTop, T, T, T))
    val nodeType = PropValue(ObjectValue(AbsNumber.alpha(DOMNode.ELEMENT_NODE), F, T, T))
    val parentNode = PropValue(ObjectValue(NullTop, F, T, T))
    val childNodes = PropValue(ObjectValue(NullTop, F, T, T))
    val firstChild = PropValue(ObjectValue(NullTop, F, T, T))
    val lastChild = PropValue(ObjectValue(NullTop, F, T, T))
    val previousSibling = PropValue(ObjectValue(NullTop, F, T, T))
    val nextSibling = PropValue(ObjectValue(NullTop, F, T, T))
    val ownerDocument = PropValue(ObjectValue(HTMLDocument.getInstance().get, F, T, T))
    val namespaceURI = PropValue(ObjectValue(NullTop, F, T, T))
    val prefix = PropValue(ObjectValue(NullTop, T, T, T))
    val localName = PropValue(ObjectValue(NullTop, F, T, T))
    val textContent = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))

    // This instance object has all properties of the Node object
    DOMNode.getInsList(nodeName, nodeValue, nodeType, parentNode, childNodes, firstChild, lastChild,
      previousSibling, nextSibling, ownerDocument, namespaceURI, prefix, localName, textContent) ++ List(
      ("tagName", tagName),
      ("scrollTop", PropValue(ObjectValue(UInt, T, T, T))),
      ("scrollLeft", PropValue(ObjectValue(UInt, T, T, T))),
      ("scrollWidth", PropValue(ObjectValue(UInt, F, T, T))),
      ("scrollHeight", PropValue(ObjectValue(UInt, F, T, T))),
      ("offsetParent", PropValue(ObjectValue(NullTop, F, T, T))),
      ("offsetTop", PropValue(ObjectValue(UInt, F, T, T))),
      ("offsetLeft", PropValue(ObjectValue(UInt, F, T, T))),
      ("offsetWidth", PropValue(ObjectValue(UInt, F, T, T))),
      ("offsetHeight", PropValue(ObjectValue(UInt, F, T, T))),
      ("clientTop", PropValue(ObjectValue(UInt, F, T, T))),
      ("clientLeft", PropValue(ObjectValue(UInt, F, T, T))),
      ("clientWidth", PropValue(ObjectValue(UInt, F, T, T))),
      ("clientHeight", PropValue(ObjectValue(UInt, F, T, T))),
      ("onclick", PropValue(ObjectValue(NullTop, T, T, T))),
      ("onload", PropValue(ObjectValue(NullTop, T, T, T)))
     )
    // TODO: schemaTypeInfo in DOM Level 3
  }
}
