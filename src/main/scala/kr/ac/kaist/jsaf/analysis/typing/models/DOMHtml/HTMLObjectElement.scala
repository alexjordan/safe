/*******************************************************************************
    Copyright (c) 2013-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing.models.DOMHtml

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import org.w3c.dom.Node
import org.w3c.dom.Element
import kr.ac.kaist.jsaf.analysis.typing.Helper
import kr.ac.kaist.jsaf.analysis.cfg.CFG
import kr.ac.kaist.jsaf.analysis.typing.models.AbsConstValue
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell

object HTMLObjectElement extends DOM {
  private val name = "HTMLObjectElement"

  /* predefined locatoins */
  val loc_cons = newSystemRecentLoc(name + "Cons")
  val loc_proto = newSystemRecentLoc(name + "Proto")
  val loc_ins = newSystemRecentLoc(name + "Ins")

  /* constructor */
  private val prop_cons: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Function")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(ObjProtoLoc), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue))),
    ("@hasinstance", AbsConstValue(PropValueNullTop)),
    ("length", AbsConstValue(PropValue(ObjectValue(Value(AbsNumber.alpha(0)), F, F, F)))),
    ("prototype", AbsConstValue(PropValue(ObjectValue(Value(loc_proto), F, F, F))))
  )
  
  /* instance */
  private val prop_ins: List[(String, AbsProperty)] = 
       HTMLElement.getInsList2() ++ List(
      ("@class",    AbsConstValue(PropValue(AbsString.alpha("Object")))),
      ("@proto",    AbsConstValue(PropValue(ObjectValue(loc_proto, F, F, F)))),
      ("@extensible", AbsConstValue(PropValue(BoolTrue))),
      // DOM Level 1
      ("form", AbsConstValue(PropValue(ObjectValue(Value(HTMLFormElement.loc_ins), F, T, T)))),
      ("code", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("align", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("archive", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("border", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("codeBase", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("codeType", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("data", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("declare", AbsConstValue(PropValue(ObjectValue(BoolTop, T, T, T)))),
      ("height", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("hspace", AbsConstValue(PropValue(ObjectValue(NumTop, T, T, T)))),
      ("name", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("standby", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("tabIndex", AbsConstValue(PropValue(ObjectValue(NumTop, T, T, T)))),
      ("type", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("useMap", AbsConstValue(PropValue(ObjectValue(StrTop, T, T, T)))),
      ("vspace", AbsConstValue(PropValue(ObjectValue(NumTop, T, T, T)))),
      ("contentDocument", AbsConstValue(PropValue(ObjectValue(Value(HTMLDocument.loc_ins), T, T, T))))
    )
  /* prorotype */
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(HTMLElement.loc_proto), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue)))
  )

  /* global */
  private val prop_global: List[(String, AbsProperty)] = List(
    (name, AbsConstValue(PropValue(ObjectValue(loc_cons, T, F, T))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global)
  )

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map()
  }

  def getPreSemanticMap(): Map[String, SemanticFun] = {
    Map()
  }

  def getDefMap(): Map[String, AccessFun] = {
    Map()
  }

  def getUseMap(): Map[String, AccessFun] = {
    Map()
  }

  /* semantics */
  // no function
     
  /* instance */
  override def getInstance(cfg: CFG): Option[Loc] = Some(newRecentLoc())
  /* list of properties in the instance object */
  override def getInsList(node: Node): List[(String, PropValue)] = node match {
    case e: Element => 
      // This object has all properties of the HTMLElement object 
      HTMLElement.getInsList(node) ++ List(
      ("@class",    PropValue(AbsString.alpha("Object"))),
      ("@proto",    PropValue(ObjectValue(loc_proto, F, F, F))),
      ("@extensible", PropValue(BoolTrue)),
      // DOM Level 1
      ("code", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("code")), T, T, T))),
      ("align", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("align")), T, T, T))),
      ("archive", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("archive")), T, T, T))),
      ("border", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("border")), T, T, T))),
      ("codeBase", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("codeBase")), T, T, T))),
      ("codeType", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("codeType")), T, T, T))),
      ("data", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("data")), T, T, T))),
      ("declare",   PropValue(ObjectValue((if(e.getAttribute("declare")=="true") T else F), T, T, T))),
      ("height", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("height")), T, T, T))),
      ("hspace",     PropValue(ObjectValue(Helper.toNumber(PValue(AbsString.alpha(e.getAttribute("hspace")))), T, T, T))),
      ("name", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("name")), T, T, T))),
      ("standby", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("standby")), T, T, T))),
      ("tabIndex",     PropValue(ObjectValue(Helper.toNumber(PValue(AbsString.alpha(e.getAttribute("tabIndex")))), T, T, T))),
      ("type", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("type")), T, T, T))),
      ("useMap", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("useMap")), T, T, T))),
      ("vspace",     PropValue(ObjectValue(Helper.toNumber(PValue(AbsString.alpha(e.getAttribute("vspace")))), T, T, T))),
      ("width", PropValue(ObjectValue(AbsString.alpha(e.getAttribute("width")), T, T, T))),
      ("form", PropValue(ObjectValue(NullTop, F, T, T))))
      // TODO: 'contentDocument' in DOM Level 2
    case _ => {
      System.err.println("* Warning: " + node.getNodeName + " cannot have instance objects.")
      List()
    }
  }
 
  def getInsList(code: PropValue, align: PropValue, archive: PropValue, border: PropValue, codeBase: PropValue, 
                 codeType: PropValue, data: PropValue, declare: PropValue, height: PropValue, hspace: PropValue, 
                 name: PropValue, standby: PropValue, tabIndex: PropValue, ttype: PropValue, useMap: PropValue, 
                 vspace: PropValue, width: PropValue, form: PropValue, xpath: PropValue): List[(String, PropValue)] = List(
    ("@class",    PropValue(AbsString.alpha("Object"))),
    ("@proto",    PropValue(ObjectValue(loc_proto, F, F, F))),
    ("@extensible", PropValue(BoolTrue)),
    // DOM Level 1
    ("code", code), 
    ("align", align),
    ("archive", archive),
    ("border", border),
    ("codeBase", codeBase),
    ("codeType", codeType),
    ("data", data),
    ("declare", declare),
    ("height", height),
    ("hspace",  hspace),
    ("name", name),
    ("standby", standby),
    ("tabIndex", tabIndex),
    ("type", ttype),
    ("useMap", useMap),
    ("vspace",  vspace),
    ("width", width),
    ("form", form),
    ("xpath", xpath)
  )
  
  override def default_getInsList(): List[(String, PropValue)] = {    
    val code = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val align = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val archive = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val border = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val codeBase = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val codeType = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val data = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val declare = PropValue(ObjectValue(BoolFalse, T, T, T))
    val height = PropValue(ObjectValue(UInt, T, T, T))
    val hspace = PropValue(ObjectValue(NumTop, T, T, T))
    val name = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val standby = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val tabIndex = PropValue(ObjectValue(NumTop, T, T, T))
    val ttype = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val useMap = PropValue(ObjectValue(AbsString.alpha(""), T, T, T))
    val vspace = PropValue(ObjectValue(NumTop, T, T, T))
    val width = PropValue(ObjectValue(UInt, T, T, T))
    val form = PropValue(ObjectValue(NullTop, F, T, T))
    val xpath = PropValue(ObjectValue(AbsString.alpha(""), F, F, F))
    // This object has all properties of the HTMLElement object 
    HTMLElement.default_getInsList ::: 
      getInsList(code, align, archive, border, codeBase, codeType, data, declare, height, hspace, name, 
                 standby, tabIndex, ttype, useMap, vspace, width, form, xpath)
  }

}
