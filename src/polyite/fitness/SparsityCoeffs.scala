package polyite.fitness

import polyite.schedule.schedule_tree.ScheduleNode
import polyite.config.Config
import polyite.schedule.schedule_tree.ScheduleNodeVisitor
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.SimpleBandNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.BandNode
import isl.Isl
import isl.Isl.TypeAliases._
import polyite.schedule.schedule_tree.ParamScheduleNodeVisitor
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.ScopInfo
import polyite.util.SCoPMetrics

/**
  * Determine the share of coefficients that are zero.
  */
abstract class SparsityCoeffs(dimT : isl.DimType) extends Feature {

  def calc(t : ScheduleNode, conf : Config, scop : ScopInfo, scopMetrics : SCoPMetrics, domInfo : DomainCoeffInfo,
    deps : Set[Dependence]) : Double = {
    val (nCoeffs : Int, nZero : Int) = t.accept(SparsityVisitor, dimT)
    val res : Double = nZero.toDouble / nCoeffs
    return res
  }
}

object SparsityIterCoeffs extends SparsityCoeffs(T_IN) {

  def getRef() : Feature = this

  def isMultiStmt() : Boolean = false

  override def toString() : String = getClass.getSimpleName
}

object SparsityParamCoeffs extends SparsityCoeffs(T_PAR) {

  def isMultiStmt() : Boolean = true

  override def toString() : String = getClass.getSimpleName
}

// (nTotal, nZero)
private object SparsityVisitor extends ParamScheduleNodeVisitor[(Int, Int), isl.DimType] {

  def visit(b : LeafNode, dimT : isl.DimType) : (Int, Int) = (0, 0)

  def visit(b : SetNode, dimT : isl.DimType) : (Int, Int) = handleInnerNode(b.getChildren, dimT)

  def handleInnerNode(children : Iterable[(Set[String], ScheduleNode)], dimT : isl.DimType) : (Int, Int) = {
    children.map(_._2).foldLeft((0, 0))((t : (Int, Int), n : ScheduleNode) => {
      val nRes : (Int, Int) = n.accept(this, dimT)
      (t._1 + nRes._1, t._2 + nRes._2)
    })
  }

  def visit(b : SeqNode, dimT : isl.DimType) : (Int, Int) = handleInnerNode(b.getChildren, dimT)

  def visit(b : DimNode, dimT : isl.DimType) : (Int, Int) = {
    val bRes : (Int, Int) = handleSchedDim(b.getSched, dimT)
    val childRes : (Int, Int) = b.getChild.accept(this, dimT)
    return (bRes._1 + childRes._1, bRes._2 + childRes._2)
  }

  def handleSchedDim(dim : isl.UnionMap, dimT : isl.DimType) : (Int, Int) = {
    val stmtScheds : List[isl.Map] = Isl.splitUnionMap(dim)
    return stmtScheds.foldLeft((0, 0))((t : (Int, Int), stmtSched : isl.Map) => {
      val coeffs : isl.Aff = Isl.islMap2Aff(stmtSched)
      val nCoeffs : Int = coeffs.dim(dimT) + (if (dimT == T_PAR) 1 else 0)
      val nZeroCoeffs : Int = (0 until coeffs.dim(dimT)).count(coeffs.getCoefficientVal(dimT, _).isZero()) + (if (dimT == T_PAR && coeffs.getConstantVal.isZero()) 1 else 0)
      (t._1 + nCoeffs, t._2 + nZeroCoeffs)
    })
  }

  def visit(b : BandNodeLoop, dimT : isl.DimType) : (Int, Int) = handleBandNode(b, dimT)

  def handleBandNode(b : BandNode, dimT : isl.DimType) : (Int, Int) = {
    val nodeRes : (Int, Int) = b.getScheds.map(_._1).foldLeft((0, 0))((t : (Int, Int), sched : isl.UnionMap) => {
      val dimRes : (Int, Int) = handleSchedDim(sched, dimT)
      (t._1 + dimRes._1, t._2 + dimRes._2)
    })
    val childRes : (Int, Int) = b.getChild.accept(this, dimT)
    return (nodeRes._1 + childRes._1, nodeRes._2 + childRes._2)
  }

  def visit(b : SimpleBandNode, dimT : isl.DimType) : (Int, Int) = handleBandNode(b, dimT)
}