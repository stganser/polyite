package polyite.schedule.schedule_tree.util

import java.util.logging.Logger

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

import isl.Conversions.convertBigIntegerToBigInt
import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Isl
import isl.Isl.TypeAliases.T_IN
import isl.IslException
import polyite.config.Config
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.BandNodeLoop
import polyite.schedule.schedule_tree.DimNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.ParamScheduleNodeVisitor
import polyite.schedule.schedule_tree.ScheduleNode
import polyite.schedule.schedule_tree.SeqNode
import polyite.schedule.schedule_tree.SetNode
import polyite.schedule.schedule_tree.SimpleBandNode

object LoopMarkVisitor {
  val myLogger : Logger = Logger.getLogger("")
}

/**
  * Mark dimensions of band nodes that actually produce loops. This transforms instances of {@code BandNode} into
  * {@code BandNodeLoop} instances.
  */
class LoopMarkVisitor extends ParamScheduleNodeVisitor[ScheduleNode, Map[String, List[Array[BigInt]]]] {

  def visit(n : LeafNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = n

  def visit(n : DimNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = throw new UnsupportedOperationException("The LoopMarkVisitor cannot process DimNodes.")

  def visit(n : SeqNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = {
    val newChildren : List[(Set[String], ScheduleNode)] = n.getChildren.map(f => (f._1, f._2.accept(this, coeffMatrices)))
    return new SeqNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }

  def visit(n : SetNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = {
    val newChildren : Set[(Set[String], ScheduleNode)] = n.getChildren.map(f => (f._1, f._2.accept(this, coeffMatrices)))
    return new SetNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }

  def handleBandNode(n : BandNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = {
    val loop = ArrayBuffer.empty[Map[String, Boolean]]
    var coeffMatricesNew = coeffMatrices
    // for each schedule Dim of this BandNode check loop generation and add result
    // to the list
    for ((sched, par) <- n.getScheds) {
      var coeffVects : HashMap[String, Array[BigInt]] = HashMap.empty
      sched.foreachMap((m : isl.Map) => {
        val stmtExp : isl.PwAff = isl.PwMultiAff.fromMap(m).getPwAff(0)
        val stmtName = m.getTupleId(T_IN).toString()
        val nInputDim : Int = m.dim(T_IN)
        val coeffVect : Array[BigInt] = new Array(nInputDim)
        var nPieces : Int = 0
        stmtExp.foreachPiece((_ : isl.Set, p : isl.Aff) => {
          if (nPieces > 0)
            throw new IllegalArgumentException("Cannot process maps that result in a piecewise affine expression.")
          nPieces += 1
          for (iterIdx <- 0 until nInputDim)
            coeffVect(iterIdx) = p.getCoefficientVal(T_IN, iterIdx).getNum
        })
        coeffVects.put(stmtName, coeffVect)
      })

      val loopMap = HashMap.empty[String, Boolean]
      for (stmt <- coeffVects.keySet) {
        var loopDim : Boolean = !coeffVects(stmt).forall(_.intValue() == 0)
        if (loopDim && !coeffMatricesNew(stmt).isEmpty) {
          val linDepSpace : isl.Set = Isl.computeLinDepSet(coeffMatricesNew(stmt), sched.getCtx) match {
            case None    => throw new IllegalStateException
            case Some(s) => s
          }
          if (Isl.islSetContains(linDepSpace, coeffVects(stmt)))
            loopDim = false
        }
        if (loopDim) {
          coeffMatricesNew += ((stmt, coeffMatricesNew(stmt) ++ List(coeffVects(stmt))))
        }
        loopMap.put(stmt, loopDim)
      }
      loop.append(loopMap.toMap)
    }
    val newChild : ScheduleNode = n.getChild.accept(this, coeffMatricesNew)
    return new BandNodeLoop(n.getDomain, n.getScheds, newChild, loop.toList, n.getCoeffMatrDims)
  }

  def visit(n : SimpleBandNode, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = handleBandNode(n, coeffMatrices)

  def visit(n : BandNodeLoop, coeffMatrices : Map[String, List[Array[BigInt]]]) : ScheduleNode = handleBandNode(n, coeffMatrices)
}