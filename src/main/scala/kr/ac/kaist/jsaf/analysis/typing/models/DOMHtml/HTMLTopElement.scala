/*******************************************************************************
    Copyright (c) 2013-2014, KAIST, S-Core.
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

package kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.Helper
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml5._
import kr.ac.kaist.jsaf.analysis.typing.models.DOMCore.{DOMElement, DOMNode}
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._

object HTMLTopElement extends DOM {
  private val name = "HTMLTopElement"
  
  /* predefined locations */
  val loc_proto = ObjProtoLoc  // dummy
  val TopElementLoc = newSystemRecentLoc(name + "Ins")
  val loc_ins = newSystemRecentLoc(name + "Ins2")
  // val loc_child = newPredefLoc("childNodesTop")
  
  private val elementList = List(
    // DOM Html
    HTMLAnchorElement, HTMLAppletElement, HTMLAreaElement, HTMLBaseElement, HTMLBaseFontElement, HTMLBodyElement,
    HTMLBRElement, HTMLButtonElement, HTMLDirectoryElement, HTMLDivElement, HTMLDListElement, HTMLFieldSetElement, 
    HTMLFontElement, HTMLFormElement, HTMLFrameElement, HTMLFrameSetElement, HTMLHeadElement, HTMLHeadingElement, 
    HTMLHRElement, HTMLHtmlElement, HTMLIFrameElement, HTMLImageElement, HTMLInputElement, HTMLIsIndexElement, 
    HTMLLabelElement, HTMLLegendElement, HTMLLIElement, HTMLLinkElement, HTMLMapElement, HTMLMenuElement, 
    HTMLMetaElement, HTMLModElement, HTMLObjectElement, HTMLOListElement, HTMLOptGroupElement, HTMLOptionElement, 
    HTMLParagraphElement, HTMLParamElement, HTMLPreElement, HTMLQuoteElement, HTMLScriptElement, HTMLSelectElement, 
    HTMLStyleElement, HTMLTableCaptionElement, HTMLTableCellElement, HTMLTableColElement, HTMLTableElement, HTMLTableRowElement,
    HTMLTableSectionElement, HTMLTextAreaElement, HTMLTitleElement, HTMLUListElement,
    // HTML 5
    HTMLCanvasElement, HTMLDataListElement, HTMLUnknownElement)

  private val elementList2 = List(
    // DOM Html
    HTMLAnchorElement, HTMLAppletElement, HTMLAreaElement, HTMLBaseElement, HTMLBaseFontElement, HTMLBodyElement,
    HTMLBRElement, HTMLButtonElement, HTMLDirectoryElement, HTMLDivElement, HTMLDListElement, HTMLFieldSetElement, 
    HTMLFontElement, HTMLFormElement, HTMLFrameElement, HTMLFrameSetElement, HTMLHeadElement, HTMLHeadingElement, 
    HTMLHRElement, HTMLHtmlElement, HTMLIFrameElement, HTMLImageElement, HTMLInputElement, HTMLIsIndexElement, 
    HTMLLabelElement, HTMLLegendElement, HTMLLIElement, HTMLLinkElement, HTMLMapElement, HTMLMenuElement, 
    HTMLMetaElement, HTMLModElement, HTMLObjectElement, HTMLOListElement, HTMLOptGroupElement, HTMLOptionElement, 
    HTMLParagraphElement, HTMLParamElement, HTMLPreElement, HTMLQuoteElement, HTMLScriptElement, HTMLSelectElement, 
    HTMLStyleElement, HTMLTableCaptionElement, HTMLTableCellElement, HTMLTableColElement, HTMLTableElement, HTMLTableRowElement,
    HTMLTableSectionElement, HTMLTextAreaElement, HTMLTitleElement, HTMLUListElement,
    // HTML 5
    HTMLCanvasElement)

  val loc_ins_set = elementList2.foldLeft(LocSetBot)((lset, e) => lset + e.loc_ins)



  val proto_locset = elementList.foldLeft(LocSetBot)((lset, e) => lset + e.loc_proto)
  
  def getInsList(link_l: Loc, childNodes_l: Loc): List[(String, PropValue)] = {
    // property value of the 'Node' interface
    val nodeName = PropValue(ObjectValue(StrTop, F, T, T))
    val nodeValue = PropValue(ObjectValue(NullTop, T, T, T))
    val nodeType = PropValue(ObjectValue(DOMNode.ELEMENT_NODE, F, T, T))
    val parentNode = PropValue(ObjectValue(Value(link_l), F, T, T))
    val childNodes = PropValue(ObjectValue(Value(childNodes_l), F, T, T))
    val firstChild = PropValue(ObjectValue(Value(link_l), F, T, T))
    val lastChild = PropValue(ObjectValue(Value(link_l), F, T, T))
    val previousSibling = PropValue(ObjectValue(Value(link_l), F, T, T))
    val nextSibling = PropValue(ObjectValue(Value(link_l), F, T, T))
    val ownerDocument = PropValue(ObjectValue(Value(HTMLDocument.GlobalDocumentLoc) + Value(NullTop), F, T, T))
    val namespaceURI = PropValue(ObjectValue(Value(StrTop) + Value(NullTop), F, T, T))
    val prefix = PropValue(ObjectValue(Value(StrTop) + Value(NullTop), T, T, T))
    val localName = PropValue(ObjectValue(Value(StrTop) + Value(NullTop), F, T, T))
    val textContent = PropValue(ObjectValue(Value(StrTop) + Value(NullTop), F, T, T))
    // This instance object has all properties of the Node interface
    val nodelist = DOMNode.getInsList(nodeName, nodeValue, nodeType, parentNode, childNodes, firstChild, lastChild,
      previousSibling, nextSibling, ownerDocument, namespaceURI, prefix, localName, textContent) 
    
    // property value of the 'Element' interface
    val tagName = PropValue(ObjectValue(Value(StrTop), F, T, T))
    val scrollTop = PropValue(ObjectValue(Value(UInt), T, T, T))
    val scrollLeft = PropValue(ObjectValue(Value(UInt), T, T, T))
    val scrollWidth = PropValue(ObjectValue(Value(UInt), F, T, T))
    val scrollHeight = PropValue(ObjectValue(Value(UInt), F, T, T))
    val offsetParent = PropValue(ObjectValue(Value(NullTop) + Value(link_l), F, T, T))
    val offsetTop = PropValue(ObjectValue(Value(UInt), F, T, T))
    val offsetLeft = PropValue(ObjectValue(Value(UInt), F, T, T))
    val offsetWidth = PropValue(ObjectValue(Value(UInt), F, T, T))
    val offsetHeight = PropValue(ObjectValue(Value(UInt), F, T, T))
    val clientTop = PropValue(ObjectValue(Value(UInt), F, T, T))
    val clientLeft = PropValue(ObjectValue(Value(UInt), F, T, T))
    val clientWidth = PropValue(ObjectValue(Value(UInt), F, T, T))
    val clientHeight = PropValue(ObjectValue(Value(UInt), F, T, T))
    val onclick = PropValue(ObjectValue(Value(NullTop), T, T, T))
    val onload = PropValue(ObjectValue(Value(NullTop), T, T, T))
    // This instance object has all properties of the Element interface
    val elementlist = DOMElement.getInsList(tagName, scrollTop, scrollLeft, scrollWidth, scrollHeight, offsetParent, 
     offsetTop, offsetLeft, offsetWidth, offsetHeight, clientTop, clientLeft, clientWidth, clientHeight, onclick, onload) 
    
    // property value of the 'HTMLElement' interface
    val id = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val title = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val lang = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val dir = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val className = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val innerHTML = PropValue(ObjectValue(Value(StrTop), T, T, T))
    val outerHTML = PropValue(ObjectValue(Value(StrTop), T, T, T))
    
    val xpath = PropValue(ObjectValue(AbsString.alpha(""), F, F, F))
    
    // This instance object has all properties of the HTMLElement interface
    val htmlelementlist = HTMLElement.getInsList(id, title, lang, dir, className, innerHTML, outerHTML, xpath) 
    nodelist ++ elementlist ++ htmlelementlist ++ List(
      ("@class", PropValue(AbsString.alpha("Object"))),
      ("@proto", PropValue(ObjectValue(Value(proto_locset), F, F, F))),
      ("@extensible", PropValue(BoolTrue))
    )

  }

  /* instance */
  private val prop_ins: List[(String, AbsProperty)] = List(
      ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
      ("@proto", AbsConstValue(PropValue(ObjectValue(Value(loc_proto), F, F, F)))),
      ("@extensible", AbsConstValue(PropValue(BoolTrue))),
      ("elements", AbsConstValue(PropValue(ObjectValue(Value(NullTop), F, F, F))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (TopElementLoc, prop_ins)
  )


  def getSemanticMap(): Map[String, SemanticFun] = {
    Map()
  }

  def getInsLoc(h: Heap): LocSet = h(TopElementLoc)(AbsString.alpha("elements"))._1._1.locs
  def setInsLoc(h: Heap, l: Loc): Heap = {
    val l_set = getInsLoc(h) 
    Helper.PropStore(h, TopElementLoc, AbsString.alpha("elements"), Value(l_set + l))
  }
  
  def setInsLoc(h: Heap, lset: LocSet): Heap = {
    val l_set = getInsLoc(h) 
    Helper.PropStore(h, TopElementLoc, AbsString.alpha("elements"), Value(l_set ++ lset))
  }

  // this object has all properites of all html elements
  override def default_getInsList(): List[(String, PropValue)] = {
    val proplist = elementList.foldLeft[List[(String, PropValue)]](List())((propl, ele) =>
      propl:::ele.default_getInsList()
    )
    // this object has all properties in DOMElement
    DOMElement.getInsList(PropValue(ObjectValue(StrTop, F, T, T))):::proplist
  }
}
