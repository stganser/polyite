package polyite.schedule.schedule_tree

import java.io.File

import scala.BigInt
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.math.BigInt.javaBigInteger2bigInt

import polyite.MainUtil
import polyite.ScopInfo
import polyite.config.Config
import polyite.config.ConfigRandLeTSeEStyle
import polyite.schedule.Dependence
import polyite.schedule.DomainCoeffInfo
import polyite.schedule.ScheduleSpaceUtils
import polyite.schedule.ScheduleUtils
import polyite.schedule.ScheduleVectorUtils
import polyite.schedule.schedule_tree.normalization.DivideCoeffsByGCDVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousDimNodesVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousSubTreesVisitor
import polyite.schedule.schedule_tree.normalization.RebuildDimSchedsVisitor
import polyite.schedule.schedule_tree.normalization.RemoveCommonOffsetVisitor

import isl.Conversions.convertLambdaToCallback1
import isl.Conversions.convertLambdaToVoidCallback1
import isl.Conversions.convertLambdaToVoidCallback2
import isl.Isl
import isl.Isl.TypeAliases.T_CST
import isl.Isl.TypeAliases.T_DIV
import isl.Isl.TypeAliases.T_IN
import isl.Isl.TypeAliases.T_OUT
import isl.Isl.TypeAliases.T_PAR
import isl.Isl.TypeAliases.T_SET
import isl.ScheduleNodeType
import polyite.schedule.schedule_tree.util.SchedTreeUtil
import polyite.schedule.schedule_tree.util.ConstructIslScheduleTreeVisitor

/**
  * Constructs Isl schedule trees from Isl union maps. The schedules must be
  * directly representable as a coefficient matrix with integral coefficients.
  *  The resulting schedule is simplified but semantically equivalent.
  */
object ScheduleTreeConstruction {

  /*
   * Tests whether the domains inner nodes and the union of the domains of their
   * children match. Rejects sequence nodes with children with non-disjunct
   * domains. The tests are implemented as assertions.
   */
  private class InspectDomainsVisitor extends ScheduleNodeVisitor[isl.UnionSet] {

    def visit(n : LeafNode) : isl.UnionSet = {
      return n.getDomain
    }

    def visit(n : SimpleBandNode) : isl.UnionSet = handleBandNodes(n)

    def visit(n : BandNodeLoop) : isl.UnionSet = handleBandNodes(n)

    private def handleBandNodes(n : BandNode) : isl.UnionSet = {
      val domainChild : isl.UnionSet = n.getChild.accept(this)
      assert(n.getDomain.isEqual(domainChild), "band node: child domain and own domain differ. \nOwn: "
        + n.getDomain + "\nChild: " + domainChild)
      return n.getDomain
    }

    def visit(n : DimNode) : isl.UnionSet = {
      val domainChild : isl.UnionSet = n.getChild.accept(this)
      assert(n.getDomain.isEqual(domainChild), "dim node: child domain and own domain differ. \nOwn: "
        + n.getDomain + "\nChild: " + domainChild)
      return n.getDomain
    }

    def visit(n : SetNode) : isl.UnionSet = {
      var collectedDomain : isl.UnionSet = isl.UnionSet.empty(n.getDomain.getSpace)
      for ((_, t) <- n.getChildren)
        collectedDomain = collectedDomain.union(t.accept(this))
      collectedDomain = Isl.simplify(collectedDomain)
      assert(n.getDomain.isEqual(collectedDomain), "set node: domain of children and own domain differ. \nOwn: "
        + n.getDomain + "\nChild: " + collectedDomain)
      return n.getDomain
    }

    def visit(n : SeqNode) : isl.UnionSet = {
      var collectedDomain : isl.UnionSet = isl.UnionSet.empty(n.getDomain.getSpace)
      for ((_, t) <- n.getChildren)
        collectedDomain = collectedDomain.union(t.accept(this))
      collectedDomain = Isl.simplify(collectedDomain)
      assert(n.getDomain.isEqual(collectedDomain), "set node: domain of children and own domain differ. \nOwn: "
        + n.getDomain + "\nChild: " + collectedDomain)
      return n.getDomain
    }
  }

  /**
    * Constructs an Isl schedule tree from the given union map representation and
    * dependences. The schedule tree representation enables transformations such
    * as tiling and strip-mining (pre-vectorization). The configuration object
    * is required to get information about the values of the SCoP's context
    * parameters. The schedules must be directly representable as a coefficient
    * matrix with integral coefficients. The resulting schedule is simplified
    * but semantically equivalent.
    */
  def islUnionMap2IslScheduleTree(schedMap : isl.UnionMap, domInfo : DomainCoeffInfo,
    scop : ScopInfo, deps : Set[Dependence], conf : Config) : isl.Schedule = {
    val schedTree = islUnionMap2ScheduleTree(schedMap, domInfo, scop, deps, conf)
    return SchedTreeUtil.scheduleTree2IslScheduleTree(schedTree)
  }

  /**
    * Constructs a schedule tree from the given union map representation and
    * dependences. The schedule tree representation enables transformations such
    * as tiling and strip-mining (pre-vectorization). The configuration object
    * is required to get information about the values of the SCoP's context
    * parameters. The schedules must be directly representable as a coefficient
    * matrix with integral coefficients. The resulting schedule is simplified
    * but semantically equivalent.
    */
  def islUnionMap2ScheduleTree(schedMap : isl.UnionMap, domInfo : DomainCoeffInfo, scop : ScopInfo, deps : Set[Dependence],
    conf : Config) : ScheduleNode = {
    var schedTree : ScheduleNode = islUnionMap2BasicScheduleTree(schedMap, domInfo, scop, deps, conf)

    if (conf.simplifySchedTrees) {
      if (conf.schedTreeSimplRebuildDimScheds)
        schedTree = schedTree.accept(new RebuildDimSchedsVisitor)
      if (conf.schedTreeSimplRemoveCommonOffset)
        schedTree = schedTree.accept(new RemoveCommonOffsetVisitor)
      //    schedTree = schedTree.accept(new ShiftOffsetToTheLeftVisitor)
      if (conf.schedTreeSimplDivideCoeffsByGCD)
        schedTree = schedTree.accept(new DivideCoeffsByGCDVisitor)
      if (conf.schedTreeSimplElimSuperfluousSubTrees)
        schedTree = schedTree.accept(new ElimSuperfluousSubTreesVisitor)
      if (conf.schedTreeSimplElimSuperfluousDimNodes)
        schedTree = schedTree.accept(new ElimSuperfluousDimNodesVisitor)
    }
    schedTree = schedTree.accept(new GroupDimsVisitor, deps.map((d : Dependence) => (d, d.map)).toMap)
    schedTree.accept(new InspectDomainsVisitor)
    return schedTree
  }

  /**
    * Construct a basic schedule tree from the given union map without applying any simplifications.
    */
  def islUnionMap2BasicScheduleTree(schedMap : isl.UnionMap, domInfo : DomainCoeffInfo, scop : ScopInfo,
    deps : Set[Dependence], conf : Config) : ScheduleNode = {
    val schedDims : List[isl.UnionMap] = Isl.splitMultiDimUnionMap(schedMap)
    val stmtIds : Set[String] = domInfo.stmtInfo.keys.toSet
    return buildBasicScheduleTree(0, schedDims, stmtIds, scop.getDomain, scop.getParams, domInfo, deps,
      conf.insertSetNodes, conf.splitLoopBodies)
  }

  /**
    * Construct a basic schedule tree from the given union map without applying any simplifications.
    */
  def islUnionMap2BasicScheduleTree(schedMap : isl.UnionMap, domInfo : DomainCoeffInfo, scop : ScopInfo,
    deps : Set[Dependence], insertSetNodes : Boolean, splitLoopBodies : Boolean) : ScheduleNode = {
    val schedDims : List[isl.UnionMap] = Isl.splitMultiDimUnionMap(schedMap)
    val stmtIds : Set[String] = domInfo.stmtInfo.keys.toSet
    return buildBasicScheduleTree(0, schedDims, stmtIds, scop.getDomain, scop.getParams, domInfo, deps,
      insertSetNodes, splitLoopBodies)
  }

  /*
   * Constructs a basic schedule tree (internal representation) without
   * permutable bands and coincidence information from the given list of
   * dimension schedules.
   */
  private def buildBasicScheduleTree(currDim : Int, schedDims : List[isl.UnionMap],
    sttmts : Set[String], domain : isl.UnionSet, context : isl.UnionSet,
    domInfo : DomainCoeffInfo, remainingDeps : Set[Dependence], insertSetNodes : Boolean, splitLoopBodies : Boolean) : ScheduleNode = {
    val currDomain : isl.UnionSet = reduceToStatements(domain, sttmts)
    val currDeps : Set[Dependence] = ScheduleSpaceUtils.filterDepSetForDomain(
      remainingDeps, currDomain)

    // Are we at the bottom of the schedule tree?
    if (currDim == schedDims.length)
      return new LeafNode(currDomain)

    // Do not reduce the schedule to the current statements right away.
    // Otherwise it doesn't correspond to domInfo
    val schedCurrDim : isl.UnionMap = simplifyOffsetWithParamEqs(schedDims(currDim), context)
    //    val schedCurrDim : isl.UnionMap = simplifyOffsetWithParamEqs(schedDims(currDim), context)
    val sttmtsPartSeq : List[Set[String]] = buildSttmtsPartitioning(sttmts, schedCurrDim, schedDims.take(currDim),
      domain)

    // The current schedule node is regular schedule dimension
    if (sttmtsPartSeq.size == 1) {
      val currSchedReduced = reduceToStatements(schedCurrDim, sttmts)

      // Check whether the current schedule node might correspond to a loop
      if (checkAllCoeffsZero(currSchedReduced, domInfo))
        // The current schedule dimension is irrelevant
        return buildBasicScheduleTree(currDim + 1, schedDims, sttmtsPartSeq.head, domain, context,
          domInfo, currDeps, insertSetNodes, splitLoopBodies)

      if (insertSetNodes) {
        // Check whether we should insert a set node
        val sttmtsPartSet : Set[Set[String]] = buildSetPartitioning(sttmtsPartSeq.head, currDeps)
        if (sttmtsPartSet.size > 1) {
          // introduce a set node
          val children : Set[(Set[String], ScheduleNode)] = (for (part : Set[String] <- sttmtsPartSet) yield {
            (part, buildBasicScheduleTree(currDim, schedDims, part, domain, context, domInfo, currDeps, insertSetNodes,
              splitLoopBodies))
          }).toSet
          return new SetNode(currDomain, children)
        }
      }

      // Special Case: additional ordering in this schedule dimension
      // optional, because Polly might refuse to apply tiling if the child of the innermost band node is not a leaf node.
      val sttmtsPartSeqConst : List[Set[String]] =
        if (splitLoopBodies)
          buildSttmtsPartitioningConst(sttmts, currSchedReduced)
        else
          List.empty
      val depsUncarried : Set[Dependence] = currDeps -- ScheduleUtils.getDepsCarriedBySchedule(currSchedReduced, currDeps)
      val (child, currSchedReduced2) = if (splitLoopBodies && sttmtsPartSeqConst.size > 1) {
        val children : List[(Set[String], ScheduleNode)] = (for (part : Set[String] <- sttmtsPartSeqConst) yield {
          (part, buildBasicScheduleTree(currDim, schedDims, part, domain, context, domInfo, depsUncarried, insertSetNodes,
            splitLoopBodies))
        }).toList
        (new SeqNode(currDomain, children), deleteConst(currSchedReduced))
      } else {
        (buildBasicScheduleTree(currDim + 1, schedDims, sttmts, domain, context, domInfo, depsUncarried, insertSetNodes,
          splitLoopBodies),
          currSchedReduced)
      }

      // this is a regular schedule dimension
      return new DimNode(currDomain, currSchedReduced2, child)
    }

    // The current schedule node is a sequence node
    val children : List[(Set[String], ScheduleNode)] = (for (part : Set[String] <- sttmtsPartSeq) yield {
      (part, buildBasicScheduleTree(currDim, schedDims, part, domain, context, domInfo, currDeps, insertSetNodes,
        splitLoopBodies))
    }).toList
    return new SeqNode(currDomain, children)
  }

  private def deleteConst(sched : isl.UnionMap) : isl.UnionMap = {
    var newUMap = isl.UnionMap.empty(sched.getSpace)
    sched.foreachMap((m : isl.Map) => {
      val stmtExp : isl.PwAff = isl.PwMultiAff.fromMap(m).getPwAff(0)
      val stmtName = m.getTupleName(T_IN)
      val nInputDim : Int = m.dim(T_IN)
      val coeffVect : Array[BigInt] = new Array(nInputDim)
      var nPieces : Int = 0

      stmtExp.foreachPiece((set : isl.Set, p : isl.Aff) => {
        if (nPieces > 0)
          throw new IllegalArgumentException("Cannot process maps that result in a piecewise affine expression.")
        nPieces += 1

        val const = p.getConstantVal
        val constAff = isl.Aff.valOnDomain(isl.LocalSpace.fromSpace(set.getSpace), const)
        val newAff = p.sub(constAff)
        val newMap = isl.Map.fromAff(newAff)
        newUMap = newUMap.addMap(newMap)
      })
    })

    return newUMap
  }

  private def buildSttmtsPartitioningConst(sttmts : Set[String], sched : isl.UnionMap) : List[Set[String]] = {
    // extract iteration coefficients and constant value
    var coeffVects : HashMap[String, (Array[BigInt], Boolean, BigInt)] = HashMap.empty
    sched.foreachMap((m : isl.Map) => {
      val stmtExp : isl.PwAff = isl.PwMultiAff.fromMap(m).getPwAff(0)
      val stmtName = m.getTupleName(T_IN)
      val nInputDim : Int = m.dim(T_IN)
      val nParDim : Int = m.dim(T_PAR)
      val coeffVect : Array[BigInt] = new Array(nInputDim)
      var const = BigInt(0)
      var param = false
      var nPieces : Int = 0
      stmtExp.foreachPiece((_ : isl.Set, p : isl.Aff) => {
        if (nPieces > 0)
          throw new IllegalArgumentException("Cannot process maps that result in a piecewise affine expression.")
        nPieces += 1
        const = p.getConstantVal.getNum
        for (parIdx <- 0 until nParDim)
          param |= !(BigInt(p.getCoefficientVal(T_PAR, parIdx).getNum).equals(BigInt(0)))
        for (iterIdx <- 0 until nInputDim)
          coeffVect(iterIdx) = p.getCoefficientVal(T_IN, iterIdx).getNum
      })
      coeffVects.put(stmtName, (coeffVect, param, const))
    })

    // If no iterations coefficients or remaining params -> skip with no sequence
    val noIts = (coeffVects.values.map(f => f._1).flatten.count(p => p != BigInt(0)) == 0)
    val params = coeffVects.values.map(f => f._2).fold(false)((a, b) => a || b)
    if (noIts || params) {
      return List(sttmts)
    }

    // Check number of Coeffs
    val numberOfCoeffPerStmt = coeffVects.values.map(f => f._1.count { p => p != BigInt(0) })
    val sameNumberOfCoeffs = (numberOfCoeffPerStmt.groupBy(f => f).keys.size == 1)

    // Check value of coeffs
    val coeffValuesPerStmt = coeffVects.values.map(_._1.filter(_ != BigInt(0))).filterNot(_.isEmpty)
    val sameCoeffValue = (coeffValuesPerStmt.flatten.groupBy(f => f).keys.size == 1)
    val coeffValue = coeffValuesPerStmt.flatten.head // is save, since if no it exists this function already exited

    // check diff between coeffs
    val constPerStmt = coeffVects.map(x => (x._1, x._2._3))
    val maxDiff = constPerStmt.maxBy(f => f._2)._2 - constPerStmt.minBy(f => f._2)._2
    val isSeq = maxDiff < coeffValue

    // if the number of Iterations coefficients or the value differs -> skip
    // if the difference of the consant value is bigger than the iteration step -> skip
    if (!sameNumberOfCoeffs || !sameCoeffValue || !isSeq)
      return List(sttmts)

    // group stmts by their constant value and return ordered list of stmts.
    val groups = constPerStmt.groupBy(f => f._2)
    return groups.map(f => (f._1, f._2.keySet)).toList.sortBy(f => f._1).map(f => f._2.toSet)
  }

  private def buildSetPartitioning(sttmts : Set[String],
    deps : Set[Dependence]) : Set[Set[String]] = {
    val withDeps : Set[String] = sttmts.filter { s =>
      deps.exists { d =>
        d.getTupleNameIn().equals(s) || d.getTupleNameOut().equals(s)
      }
    }
    val withoutDeps : Set[String] = sttmts -- withDeps
    var result : Set[Set[String]] = Set.empty
    if (!withDeps.isEmpty)
      result += withDeps
    if (!withoutDeps.isEmpty)
      result += withoutDeps
    return result
  }

  private def checkAllCoeffsZero(sched : isl.UnionMap, domInfo : DomainCoeffInfo) : Boolean = {
    val coeffMatrix : List[Array[BigInt]] = ScheduleUtils.islUnionMap2CoeffMatrix(domInfo, sched)
    return coeffMatrix.forall(ScheduleVectorUtils.checkAllZeroBigInt(_))
  }

  def simplifyOffsetWithParamEqs(sched : isl.UnionMap, context : isl.UnionSet) : isl.UnionMap = {

    val eqMatrix : isl.Mat = isl.Set.fromUnionSet(context).getBasicSetList.getBasicSet(0)
      .detectEqualities().equalitiesMatrix(T_CST, T_PAR, T_SET, T_DIV)

    if (eqMatrix == null)
      return sched

    var newSched : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)

    sched.foreachMap((m : isl.Map) => {
      val blankConstr : isl.Constraint = isl.Constraint.allocEquality(isl.LocalSpace.fromSpace(m.getSpace))
      val mWithEqs : isl.Map = (0 until eqMatrix.rows()).view.foldLeft(m)(((mm : isl.Map), (row : Int)) => {

        val paramEquality : isl.Constraint = (0 until m.dim(T_PAR)).view.foldLeft(blankConstr.setConstantVal(eqMatrix.getElementVal(row, 0)))(
          ((constr : isl.Constraint), (paramIdx : Int)) => {
            constr.setCoefficientVal(T_PAR, paramIdx, eqMatrix.getElementVal(row, paramIdx + 1))
          })
        mm.addConstraint(paramEquality)
      })
      isl.PwMultiAff.fromMap(mWithEqs).foreachPiece((_ : isl.Set, mAff : isl.MultiAff) => {
        newSched = newSched.addMap(isl.Map.fromMultiAff(mAff))
      })
    })
    return newSched
  }

  private def buildSttmtsPartitioning(sttmts : Set[String], sched : isl.UnionMap, schedPrefix : List[isl.UnionMap],
    domain : isl.UnionSet) : List[Set[String]] = {
    var remSttmts : Set[String] = sttmts
    var result : List[Set[String]] = List.empty

    while (!remSttmts.isEmpty) {
      val minMaybe : Option[String] = getLeqElemIfExists(remSttmts, sched, schedPrefix, domain)
      if (minMaybe.isDefined) {
        val min : String = minMaybe.get
        var rest = remSttmts - min
        val eqToMin = rest.filter { s =>
          {
            val order = SchedTreeUtil.calcOrder(sched, schedPrefix, s, min, domain)
            !order.isDefined || !(order.get.isNegativeOnly || order.get.isPositiveOnly)
          }
        } + min
        rest = rest -- eqToMin
        //        var areEqual : Boolean = true
        //        for (s <- eqToMin)
        //          for (t <- eqToMin)
        //            if (s < t) {
        //              val order = calcOrder(sched, schedPrefix, s, t, domain)
        //              if (order.isDefined && (order.get.isPositiveOnly || order.get.isNegativeOnly))
        //                areEqual = false
        //            }
        var happenBeforeRest = true
        for (s <- eqToMin)
          for (t <- rest) {
            val order = SchedTreeUtil.calcOrder(sched, schedPrefix, s, t, domain)
            if (order.isDefined && !order.get.isPositiveOnly)
              happenBeforeRest = false
          }
        //        if (areEqual && happenBeforeRest) {
        if (happenBeforeRest) {
          result ::= eqToMin
          remSttmts --= eqToMin
        } else {
          result ::= remSttmts.toSet
          return result.reverse
        }
      } else {
        result ::= remSttmts.toSet
        return result.reverse
      }
    }
    return result.reverse
  }

  private def getLeqElemIfExists(sttmts : Set[String], sched : isl.UnionMap, schedPrefix : List[isl.UnionMap],
    domain : isl.UnionSet) : Option[String] = {
    for (s <- sttmts) {
      val rest : Set[String] = sttmts - s
      val isMin = rest.forall { t =>
        {
          val order = SchedTreeUtil.calcOrder(sched, schedPrefix, s, t, domain)
          !order.isDefined || !order.get.isNegative
        }
      }
      if (isMin)
        return Some(s)
    }
    return None
  }

  private def reduceToStatements(sched : isl.UnionMap, sttmts : Set[String]) : isl.UnionMap = {
    var result : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)
    sched.foreachMap((m : isl.Map) => {
      if (sttmts.contains(m.getTupleName(T_IN)))
        result = result.addMap(m)
    })
    return result
  }

  private def reduceToStatements(domain : isl.UnionSet, sttmts : Set[String]) : isl.UnionSet = {
    var result : isl.UnionSet = isl.UnionSet.empty(domain.getSpace)
    domain.foreachSet((s : isl.Set) => {
      if (sttmts.contains(s.getTupleName()))
        result = result.addSet(s)
    })
    return result
  }
}
