package polyite.schedule.schedule_tree.util

import polyite.schedule.ScheduleUtils.CmpMultiResult
import isl.Isl
import isl.Isl.TypeAliases._
import isl.Conversions._
import polyite.schedule.schedule_tree.ScheduleNode
import isl.ScheduleNodeType._
import polyite.schedule.Dependence
import polyite.schedule.schedule_tree.normalization.RebuildDimSchedsVisitor
import polyite.schedule.schedule_tree.normalization.RemoveCommonOffsetVisitor
import polyite.schedule.schedule_tree.normalization.DivideCoeffsByGCDVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousSubTreesVisitor
import polyite.schedule.schedule_tree.normalization.ElimSuperfluousDimNodesVisitor
import polyite.schedule.schedule_tree.GroupDimsVisitor
import polyite.schedule.schedule_tree.BandNode
import polyite.schedule.schedule_tree.LeafNode
import polyite.schedule.schedule_tree.SeqNode

object SchedTreeUtil {
  def calcOrder(sched : isl.UnionMap, schedPrefix : List[isl.UnionMap], s1 : String, s2 : String,
    domain : isl.UnionSet) : Option[CmpMultiResult] = {
    Isl.check1D(sched)

    var schedSimpl : isl.UnionMap = sched.coalesce()

    // Extract the domains of both statements
    var s1Domain : isl.Set = null
    var s2Domain : isl.Set = null
    domain.foreachSet((s : isl.Set) => {
      if (s.getTupleName.equals(s1))
        s1Domain = s
      if (s.getTupleName.equals(s2))
        s2Domain = s
    })

    def checkDomainFound(d : isl.Set, s : String) {
      if (d == null)
        throw new IllegalArgumentException("Couldn't find domain for statement " + s)
    }
    checkDomainFound(s1Domain, s1)
    checkDomainFound(s2Domain, s2)

    // Extract the schedules for both statements
    var s1Sched : isl.Map = null
    var s2Sched : isl.Map = null
    sched.foreachMap((m : isl.Map) => {
      if (m.getTupleName(T_IN).equals(s1))
        s1Sched = m
      if (m.getTupleName(T_IN).equals(s2))
        s2Sched = m
    })

    def checkSchedFound(m : isl.Map, s : String) {
      if (m == null)
        throw new IllegalArgumentException("Couldn't find schedule for statement " + s)
    }
    checkSchedFound(s1Sched, s1)
    checkSchedFound(s2Sched, s2)

    // Extract the schedule prefixes of both schedules
    def extractSchedPrefix(stmt : String) : List[isl.Map] = {
      schedPrefix.map { (dimSched : isl.UnionMap) =>
        {
          var sSched : isl.Map = null
          dimSched.foreachMap((m : isl.Map) => {
            if (m.getTupleName(T_IN).equals(stmt))
              sSched = m
          })
          checkSchedFound(sSched, stmt)
          sSched
        }
      }
    }

    val s1SchedPrefix : List[isl.Map] = extractSchedPrefix(s1)
    val s2SchedPrefix : List[isl.Map] = extractSchedPrefix(s2)
    val sameExecTimeRel : isl.Map = buildSameExecTimeRelation(s1SchedPrefix, s2SchedPrefix, s1Domain, s2Domain)

    if (sameExecTimeRel.isEmpty())
      return None

    val sameExecTimeRelSchedApplied = sameExecTimeRel.applyDomain(s1Sched).applyRange(s2Sched)
    val deltas : isl.Set = sameExecTimeRelSchedApplied.deltas()
    val deltaSpace : isl.Space = deltas.getSpace
    val univ : isl.Set = isl.Set.universe(deltaSpace)
    val ctx : isl.Ctx = univ.getCtx
    val zer = univ.fixVal(T_SET, 0, isl.Val.zero(ctx))
    val pos = univ.lowerBoundVal(T_SET, 0, isl.Val.one(ctx))
    val neg = univ.upperBoundVal(T_SET, 0, isl.Val.negone(ctx))

    val isNeg = !deltas.intersect(neg).isEmpty()
    val isZer = !deltas.intersect(zer).isEmpty()
    val isPos = !deltas.intersect(pos).isEmpty()
    return Some(CmpMultiResult(isNeg, isZer, isPos))
  }

  private def buildSameExecTimeRelation(schedPref1 : List[isl.Map], schedPref2 : List[isl.Map], dom1 : isl.Set, dom2 : isl.Set) : isl.Map = {
    var result : isl.Map = isl.Map.fromDomainAndRange(dom1, dom2)
    schedPref1.zip(schedPref2).view.foreach { (t : (isl.Map, isl.Map)) =>
      {
        val constr = Isl.buildEqConstrFromMaps(dom1, dom2, t._1, t._2)
        result = result.addConstraint(constr)
      }
    }
    return result
  }

  /**
    * Mark dimensions of band nodes that actually produce loops. This transforms instances of {@code BandNode} into
    * {@code BandNodeLoop} instances.
    */
  def markLoops(n : ScheduleNode) : ScheduleNode = {
    val coeffMatrices : Map[String, List[Array[BigInt]]] = Isl.islUnionSetGetTupleNames(n.getDomain).map((_, List.empty)).toMap

    return n.accept(new LoopMarkVisitor, coeffMatrices)
  }

  /**
    * Until version 3.9.1 a band node is tiled by Polly iff its child is a leaf node. Since version 4 a band node is also
    * tiled if its child is a sequence node with only leafs as its children. This resembles fusion of the innermost loops.
    */
  def isTiledByPolly(t : BandNode, pollyVersionGeq4 : Boolean) : Boolean = {
    return t.getChild.isInstanceOf[LeafNode] || (pollyVersionGeq4 && isInnerSeqNode(t.getChild))
  }

  /**
    * Check whether {@code s} is sequence node and all of its children are leaf nodes.
    */
  def isInnerSeqNode(s : ScheduleNode) : Boolean = {
    if (s.isInstanceOf[SeqNode]) {
      val sSeqNode : SeqNode = s.asInstanceOf[SeqNode]
      return sSeqNode.getChildren.forall(_._2.isInstanceOf[LeafNode])
    }
    return false
  }

  /**
    * Converts a schedule tree into an Isl schedule tree.
    */
  def scheduleTree2IslScheduleTree(t : ScheduleNode) : isl.Schedule = {
    val islSched : isl.Schedule = t.accept(new ConstructIslScheduleTreeVisitor)
    return markBandsPermutable(islSched)
  }

  // Call once on the otherwise complete Isl-schedule
  private def markBandsPermutable(s : isl.Schedule) : isl.Schedule = {
    return s.mapScheduleNodeBottomUp((n : isl.ScheduleNode) => {
      if (n.getType.equals(isl.ScheduleNodeType.NodeBand) && n.bandNMember() > 1)
        n.bandSetPermutable(1)
      else
        n
    })
  }

  /**
    * Tiles the given schedule. Tiling is applied according to the first level tiling of Polly. We only tile the
    * innermost band node. Tiling can only be applied to that node of its child is a leaf node. Optionally, the child
    * node may also be a sequence node that has only leaf nodes as its children. All dimensions of the band are tiled
    * with the given standard tile size.
    */
  def tileSchedule(sched : isl.Schedule, defaultTileSize : Int, allowInnerSeq : Boolean) : isl.Schedule = {
    val ctx : isl.Ctx = sched.getCtx
    return sched.mapScheduleNodeBottomUp((n : isl.ScheduleNode) => {
      n.getType match {
        case NodeBand => {
          if (n.bandGetPermutable() && n.bandNMember() > 1 && isInnermostBand(n, allowInnerSeq)) {
            val nMemb : Int = n.bandNMember()
            var tileSizesList : isl.ValList = isl.ValList.alloc(ctx, 0)
            val tileSize : isl.Val = isl.Val.fromInt(ctx, 32)
            for (i <- 0 until nMemb)
              tileSizesList = tileSizesList.add(tileSize)
            val sp : isl.Space = isl.Space.setAlloc(ctx, 0, nMemb)
            val tileSizes : isl.MultiVal = isl.MultiVal.fromValList(sp, tileSizesList)
            n.bandTile(tileSizes)
          } else {
            n
          }
        }
        case _ => n
      }
    })
  }

  private def isInnermostBand(n : isl.ScheduleNode, allowInnerSeq : Boolean) : Boolean = {
    val child : isl.ScheduleNode = n.firstChild()
    child.getType match {
      case NodeLeaf => return true
      case NodeSequence => {
        for (i : Int <- 0 until n.nChildren())
          if (child.getChild(i).getChild(0).getType != NodeLeaf) {
            return false
          }
        return allowInnerSeq
      }
      case _ => return false
    }
  }

  /**
    * Simplifies the given schedule tree as far as possible and detect permutable bands.
    */
  def simplifySchedTree(s : ScheduleNode, deps : Set[Dependence]) : ScheduleNode = {
    return s.accept(new RebuildDimSchedsVisitor)
      .accept(new RemoveCommonOffsetVisitor)
      .accept(new DivideCoeffsByGCDVisitor)
      .accept(new ElimSuperfluousSubTreesVisitor)
      .accept(new ElimSuperfluousDimNodesVisitor)
      .accept(new GroupDimsVisitor, deps.map((d : Dependence) => (d, d.map.detectEqualities())).toMap)
  }
}