package polyite

class ScopInfo {
  private var jscop : Any = null
  private var params : isl.UnionSet = null
  private var domain : isl.UnionSet = null
  private var sched : isl.UnionMap = null
  private var rds : isl.UnionMap = null
  private var wrs : isl.UnionMap = null

  def copy : ScopInfo = {
    val cp = new ScopInfo()
    cp.jscop = jscop
    cp.params = params
    cp.domain = domain
    cp.sched = sched
    cp.rds = rds
    cp.wrs = wrs
    return cp
  }

  def setJSCOP(jscop : Any) : ScopInfo = {
    val cp = copy
    cp.jscop = jscop
    return cp
  }

  def setParams(params : isl.UnionSet) : ScopInfo = {
    val cp = copy
    cp.params = params
    return cp
  }

  def addDomain(newDomain : isl.Set) : ScopInfo = {
    val cp = copy

    if (domain == null) {
      cp.domain = isl.UnionSet.fromSet(newDomain)
    } else {
      cp.domain = domain.addSet(newDomain)
    }
    return cp
  }

  def addSchedule(newSchedule : isl.Map) : ScopInfo = {
    val cp = copy
    cp.sched = isl.Isl.addMapToUnionMap(sched, newSchedule)
    return cp
  }

  def addRds(newRds : isl.Map) : ScopInfo = {
    val cp = copy
    cp.rds = isl.Isl.addMapToUnionMap(rds, newRds)
    return cp
  }

  def addWrs(newWrs : isl.Map) : ScopInfo = {
    val cp = copy
    cp.wrs = isl.Isl.addMapToUnionMap(wrs, newWrs)
    return cp
  }

  override def toString() : String = {
    val sb : StringBuilder = new StringBuilder
    sb.append("params: ").append(params).append('\n')
      .append("domain: ").append(domain).append('\n')
      .append("sched: ").append(sched).append('\n')
      .append("rds: ").append(rds).append('\n')
      .append("wrs: ").append(wrs).append('\n')
      .toString()
  }

  def transferToCtx(newCtx : isl.Ctx) : ScopInfo = {
    val copy : ScopInfo = new ScopInfo()
    copy.jscop = jscop
    copy.params = isl.UnionSet.readFromStr(newCtx, params.toString)
    copy.domain = isl.UnionSet.readFromStr(newCtx, domain.toString)
    copy.sched = isl.UnionMap.readFromStr(newCtx, sched.toString)
    copy.rds = isl.UnionMap.readFromStr(newCtx, rds.toString)
    copy.wrs = isl.UnionMap.readFromStr(newCtx, wrs.toString)
    return copy
  }

  def getJscop : Any = jscop
  def getParams : isl.UnionSet = params
  def getDomain : isl.UnionSet = domain
  def getSched : isl.UnionMap = sched
  def getRds : isl.UnionMap = rds
  def getWrs : isl.UnionMap = wrs
}