/*******************************************************************************
    Copyright (c) 2012-2014, S-Core, KAIST.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ***************************************************************************** */

package kr.ac.kaist.jsaf.analysis.typing.models

import kr.ac.kaist.jsaf.analysis.cfg._
import kr.ac.kaist.jsaf.analysis.typing.domain._
import kr.ac.kaist.jsaf.analysis.typing.models.builtin._
import kr.ac.kaist.jsaf.analysis.typing._
import scala.Some
import kr.ac.kaist.jsaf.analysis.typing.domain.Heap
import kr.ac.kaist.jsaf.analysis.typing.domain.Context

class BuiltinModel(cfg: CFG) extends Model(cfg)  {

  private val list_builtin = List[ModelData](
    BuiltinArray, BuiltinBoolean, BuiltinDate, BuiltinError, BuiltinFunction, BuiltinGlobal,
    BuiltinJSON, BuiltinMath, BuiltinNumber, BuiltinObject, BuiltinRegExp, BuiltinString
  )

  private var map_fid = Map[FunctionId, String]()
  private var map_semantic = Map[String, SemanticFun]()

  def initialize(h: Heap): Heap = {
    /* init function map */
    map_semantic = list_builtin.foldLeft(map_semantic)((m, builtin) => m ++ builtin.getSemanticMap())

    /* init api objects */
    val h_1 = list_builtin.foldLeft(h)((h1, builtin) =>
      builtin.getInitList().foldLeft(h1)((h2, lp) => {
        /* List[(String, PropValue, Option[(Loc, Obj)], Option[FunctionId] */
        val list_props = lp._2.map((x) => prepareForUpdate("Builtin", x._1, x._2))
        /* update api function map */
        list_props.foreach((v) =>
          v._4 match {
            case Some((fid, name)) => {map_fid = map_fid + (fid -> name)}
            case None => Unit
          })
        /* api object */
        val obj = h2.map.get(lp._1) match {
          case Some(o) =>
            list_props.foldLeft(o)((oo, pv) => oo.update(pv._1, pv._2))
          case None =>
            list_props.foldLeft(Obj.empty)((o, pvo) => o.update(pvo._1, pvo._2))
        }
        /* added function object to heap if any*/
        val heap = list_props.foldLeft(h2)((h3, pvo) => pvo._3 match {
          case Some((l, o)) => Heap(h3.map.updated(l, o))
          case None => h3
        })

        /* added api object to heap */
        Heap(heap.map.updated(lp._1, obj))
      })
    )

    // Date.prototype.toGMTString : ECMAScript 5 Appendix B.2.6
    // Date.prototype.toGMTString has the same function object as the one of Date.prototype.toUTCString
    val date_protoobj = h_1(BuiltinDate.ProtoLoc)
    val new_protoobj = date_protoobj.update(AbsString.alpha("toGMTString"), date_protoobj("toUTCString"))
    val h_2 = h_1.update(BuiltinDate.ProtoLoc, new_protoobj)

    h_2
  }
  def addAsyncCall(cfg: CFG, loop_head: Node): (List[Node],List[Node]) = (List(),List())
  def isModelFid(fid: FunctionId) = map_fid.contains(fid)
  def getFIdMap(): Map[FunctionId, String] = map_fid
  def getSemanticMap(): Map[String, SemanticFun] = map_semantic

  def asyncSemantic(sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG,
                    name: String, list_addr: List[Address]): ((Heap, Context), (Heap, Context)) = {
    ((HeapBot, ContextBot),(HeapBot, ContextBot))
  }
  def asyncPreSemantic(sem: Semantics, h: Heap, ctx: Context, he: Heap, ctxe: Context, cp: ControlPoint, cfg: CFG,
                       name: String, list_addr: List[Address]): (Heap, Context) = {
    (HeapBot, ContextBot)
  }
  def asyncDef(h: Heap, ctx: Context, cfg: CFG, name: String, list_addr: List[Address]): LPSet = {
    LPBot
  }
  def asyncUse(h: Heap, ctx: Context, cfg: CFG, name: String, list_addr: List[Address]): LPSet = {
    LPBot
  }
  def asyncCallgraph(h: Heap, inst: CFGInst, map: Map[CFGInst, Set[FunctionId]],
                     name: String, list_addr: List[Address]): Map[CFGInst, Set[FunctionId]] = {
    Map()
  }
}
