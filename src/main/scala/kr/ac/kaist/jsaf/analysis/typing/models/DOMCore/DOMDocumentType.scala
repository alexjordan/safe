/*******************************************************************************
    Copyright (c) 2013, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf.analysis.typing.models.DOMCore

import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.domain.{BoolFalse => F, BoolTrue => T}
import kr.ac.kaist.jsaf.analysis.typing.models._
import org.w3c.dom.Node
import org.w3c.dom.DocumentType
import kr.ac.kaist.jsaf.analysis.cfg.CFG
import kr.ac.kaist.jsaf.analysis.typing.models.AbsConstValue
import kr.ac.kaist.jsaf.analysis.typing.AddressManager._
import kr.ac.kaist.jsaf.Shell

object DOMDocumentType extends DOM {
  private val name = "DocumentType"

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
   
  /* instance */
  private val prop_ins: List[(String, AbsProperty)] = 
       DOMNode.getInsList2() ++ List(
      ("@class",    AbsConstValue(PropValue(AbsString.alpha("Object")))),
      ("@proto",    AbsConstValue(PropValue(ObjectValue(loc_proto, F, F, F)))),
      ("@extensible", AbsConstValue(PropValue(BoolTrue))),
      // DOM Level 1
      ("name", AbsConstValue(PropValue(ObjectValue(StrTop, F, T, T)))),
      ("entities", AbsConstValue(PropValue(ObjectValue(Value(DOMNamedNodeMap.loc_ins), F, T, T)))),
      ("notations", AbsConstValue(PropValue(ObjectValue(Value(DOMNamedNodeMap.loc_ins), F, T, T)))),
      ("publicId", AbsConstValue(PropValue(ObjectValue(StrTop, F, T, T)))),
      ("systemId", AbsConstValue(PropValue(ObjectValue(StrTop, F, T, T)))),
      ("internalSubset", AbsConstValue(PropValue(ObjectValue(StrTop, F, T, T))))
    )


  /* prorotype */
  private val prop_proto: List[(String, AbsProperty)] = List(
    ("@class", AbsConstValue(PropValue(AbsString.alpha("Object")))),
    ("@proto", AbsConstValue(PropValue(ObjectValue(Value(DOMNode.loc_proto), F, F, F)))),
    ("@extensible", AbsConstValue(PropValue(BoolTrue)))
  )

  /* global */
  private val prop_global: List[(String, AbsProperty)] = List(
    (name, AbsConstValue(PropValue(ObjectValue(loc_cons, T, F, T))))
  )

  def getInitList(): List[(Loc, List[(String, AbsProperty)])] = if(Shell.params.opt_Dommodel2) List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global), (loc_ins, prop_ins)

  ) else List(
    (loc_cons, prop_cons), (loc_proto, prop_proto), (GlobalLoc, prop_global)  ) 

  def getSemanticMap(): Map[String, SemanticFun] = {
    Map()
  }

  /* semantics */
  // no function

  /* instance */
  override def getInstance(cfg: CFG): Option[Loc] = Some(newRecentLoc())
  /* list of properties in the instance object */
  override def getInsList(node: Node): List[(String, PropValue)] = node match {
    case d: DocumentType => 
      val name = d.getName
      val publicId = d.getPublicId
      val systemId = d.getSystemId
      val internalSubset = d.getInternalSubset
      // This instance object has all properties of the Node object
      DOMNode.getInsList(node) ++ List(
      ("@class",  PropValue(AbsString.alpha("Object"))),
      ("@proto",  PropValue(ObjectValue(loc_proto, F, F, F))),
      ("@extensible", PropValue(BoolTrue)),
      // DOM Level 1
      ("name",   PropValue(ObjectValue(AbsString.alpha(if(name!=null) name else ""), F, T, T))),
      // Introduced in DOM Level 2
      ("publicId",   PropValue(ObjectValue(AbsString.alpha(if(publicId!=null) publicId else ""), F, T, T))),
      ("systemId",   PropValue(ObjectValue(AbsString.alpha(if(systemId!=null) systemId else ""), F, T, T))),
      ("internalSubset",   PropValue(ObjectValue(AbsString.alpha(if(internalSubset!=null) internalSubset else ""), F, T, T))))
      // TODO: 'entities', 'notations' in DOM Level 1
    case _ => {
      System.err.println("* Warning: " + node.getNodeName + " cannot be an instance of DocumentType.")
      List()
    }
  }
}
