package polyite.schedule

import isl.BasicMap
import isl.Isl.TypeAliases._

//class Dependence(val map : isl.BasicMap, val weakConstr : isl.BasicSet,
//    val strongConstr : isl.BasicSet) {

class Dependence private () {
  private var m : isl.BasicMap = null;
  private var wConstr : isl.BasicSet = null
  private var sConstr : isl.BasicSet = null

  private var mStr : String = null;
  private var wConstrStr : String = null
  private var sConstrStr : String = null
  
  private var tupleNameIn : String = null;
  
  private var tupleNameOut : String = null;

  private def this(map : isl.BasicMap, wConstr : isl.BasicSet,
    sConstr : isl.BasicSet, mStr : String, wConstrStr : String,
    sConstrStr : String, tupleNameIn : String, tupleNameOut : String) = {
    this()
    this.m = map
    this.wConstr = wConstr
    this sConstr = sConstr
    this.mStr = mStr
    this.wConstrStr = wConstrStr
    this.sConstrStr = sConstrStr
    this.tupleNameIn = tupleNameIn
    this.tupleNameOut = tupleNameOut
  }

  def this(map : isl.BasicMap, weakConstr : isl.BasicSet, strongConstr : isl.BasicSet) = this(
    map, weakConstr, strongConstr, map.toString(), weakConstr.toString(), strongConstr.toString(), map.getTupleName(T_IN), map.getTupleName(T_OUT))

  def map : isl.BasicMap = m
  
  def weakConstr : isl.BasicSet = wConstr
  
  def strongConstr : isl.BasicSet = sConstr
    
  override def toString() : String = mStr

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[Dependence]) {
      return o.asInstanceOf[Dependence].mStr.equals(mStr)
    }
    return false
  }

  override def hashCode() : Int = {
    return mStr.hashCode()
  }
  
  def getTupleNameIn() : String = tupleNameIn
  
  def getTupleNameOut() : String = tupleNameOut
  
  def transferToCtx(ctx : isl.Ctx) : Dependence = {
    return new Dependence(isl.BasicMap.readFromStr(ctx, mStr),
        isl.BasicSet.readFromStr(ctx, wConstrStr),
        isl.BasicSet.readFromStr(ctx, sConstrStr), mStr, wConstrStr, sConstrStr, tupleNameIn, tupleNameOut)
  }
}