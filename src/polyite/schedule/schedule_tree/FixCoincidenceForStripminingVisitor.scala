package polyite.schedule.schedule_tree

object FixCoincidenceForStripminingVisitor extends ScheduleNodeVisitorLeaveNDegUnchanged {
  
  def visit(b: DimNode): ScheduleNode = b
  
  def visit(b: BandNodeLoop): ScheduleNode = {
    if (b.getDomain.nSet() == 1)
      return b
    val newScheds : List[(isl.UnionMap, Boolean)] = b.getScheds.map((t : (isl.UnionMap, Boolean)) => (t._1, false))
    val newChild = b.getChild.accept(this)
    return new BandNodeLoop(b.getDomain, newScheds, newChild, b.getLoop, b.getCoeffMatrDims)
  }
  
  def visit(b: SimpleBandNode): ScheduleNode = {
    if (b.getDomain.nSet() == 1)
      return b
    val newScheds : List[(isl.UnionMap, Boolean)] = b.getScheds.map((t : (isl.UnionMap, Boolean)) => (t._1, false))
    val newChild = b.getChild.accept(this)
    return new SimpleBandNode(b.getDomain, newScheds, newChild, b.getCoeffMatrDims)
  }
  
  def visit(b: LeafNode): ScheduleNode = b
}