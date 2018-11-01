package polyite.schedule.schedule_tree

import isl.Conversions.convertLambdaToVoidCallback1
import isl.Isl.TypeAliases.T_IN
import isl.Isl

trait ScheduleNode {
  protected var father : ScheduleNode = null
  protected var domain : isl.UnionSet = null
  protected var coeffMatrDims : List[Int] = null
  def setFather(father : ScheduleNode) {
    this.father = father
  }
  def isRoot : Boolean = father == null
  def getFather : ScheduleNode = father
  def getDomain : isl.UnionSet = domain
  def getCoeffMatrDims : List[Int] = coeffMatrDims

  /*
     * schedule for given statements from root until this node. Fails if stmts is not a subset of the
     * domain of this node.
     */
  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap]

  def accept[T](v : ScheduleNodeVisitor[T]) : T

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T
}

class SeqNode private () extends ScheduleNode {

  private var children : List[(Set[String], ScheduleNode)] = null

  def this(domain : isl.UnionSet, children : List[(Set[String], ScheduleNode)], coeffMatrDims : List[Int]) = {
    this()
    this.domain = domain
    this.children = children
    this.coeffMatrDims = coeffMatrDims
    children.foreach((t : (Set[String], ScheduleNode)) => t._2.setFather(this))
  }

  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap] = {
    if (father == null)
      return List.empty
    return father.getSchedulePrefix(stmts)
  }

  def getChildren : List[(Set[String], ScheduleNode)] = children

  def accept[T](v : ScheduleNodeVisitor[T]) : T = {
    return v.visit(this)
  }

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = {
    return v.visit(this, param)
  }

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[SeqNode]) {
      val oSeqNode : SeqNode = o.asInstanceOf[SeqNode]
      var equal = this.isRoot == oSeqNode.isRoot
      equal &&= this.domain.isEqual(oSeqNode.domain)
      equal &&= this.children.size == oSeqNode.children.size
      return equal && this.children.zip(oSeqNode.children).forall(t => t._1.equals(t._2))
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = Isl.islUnionSetUniqueToString(this.domain).hashCode()
    hash = hash * prime + this.children.hashCode()
    return hash
  }

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    val childStrs : Iterable[String] = children.map(t => {
      StringBuilder.newBuilder.append("Filter").append(t._1.toList.sorted.mkString("(", ",", ")")).append(";").append(t._2).toString()
    })
    sb.append("SeqNode").append(childStrs.mkString("(", ",", ")"))
    return sb.toString()
  }
}

class SetNode private () extends ScheduleNode {

  private var children : Set[(Set[String], ScheduleNode)] = null

  def this(domain : isl.UnionSet, children : Set[(Set[String], ScheduleNode)], coeffMatrDims : List[Int]) = {
    this()
    this.domain = domain
    this.children = children
    this.coeffMatrDims = coeffMatrDims

    children.foreach((t : (Set[String], ScheduleNode)) => t._2.setFather(this))
  }

  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap] = {
    if (father == null)
      return List.empty
    return father.getSchedulePrefix(stmts)
  }

  def getChildren : Set[(Set[String], ScheduleNode)] = children

  def accept[T](v : ScheduleNodeVisitor[T]) : T = {
    return v.visit(this)
  }

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = {
    return v.visit(this, param)
  }

  override def equals(o : Any) : Boolean = {
    implicit val childOrd = new Ordering[(Set[String], ScheduleNode)] {
      override def compare(t1 : (Set[String], ScheduleNode), t2 : (Set[String], ScheduleNode)) : Int = {
        t1._1.toList.sorted.toString compare t2._1.toList.sorted.toString
      }
    }

    if (o.isInstanceOf[SetNode]) {
      val oSetNode : SetNode = o.asInstanceOf[SetNode]
      var equal = this.isRoot == oSetNode.isRoot
      equal &&= this.domain.isEqual(oSetNode.domain)
      equal &&= this.children.size == oSetNode.children.size
      if (equal) {
        val myChildrenSorted : List[(Set[String], ScheduleNode)] = this.children.toList.sorted
        val otherChildrenSorted : List[(Set[String], ScheduleNode)] = oSetNode.children.toList.sorted
        return equal && myChildrenSorted.zip(otherChildrenSorted).forall(t => t._1.equals(t._2))
      }
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = Isl.islUnionSetUniqueToString(this.domain).hashCode()
    hash = hash * prime + this.children.hashCode()
    return hash
  }

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    val childStrs : Iterable[String] = children.toList.sortBy(_._1.toList.sorted.toString).map(t => {
      StringBuilder.newBuilder.append("Filter").append(t._1.toList.sorted.mkString("(", ",", ")")).append(";").append(t._2).toString()
    })
    sb.append("SetNode").append(childStrs.mkString("(", ",", ")"))
    return sb.toString()
  }
}

class DimNode private () extends ScheduleNode {

  private var sched : isl.UnionMap = null
  private var child : ScheduleNode = null

  def this(domain : isl.UnionSet, sched : isl.UnionMap, child : ScheduleNode, coeffMatrDims : List[Int]) = {
    this()
    this.domain = domain
    this.sched = sched
    this.child = child
    this.coeffMatrDims = coeffMatrDims
    child.setFather(this)
  }

  def getSched : isl.UnionMap = sched
  def getChild : ScheduleNode = child

  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap] = {
    var s : isl.UnionMap = isl.UnionMap.empty(sched.getSpace)

    sched.foreachMap((m : isl.Map) => {
      if (stmts.contains(m.getTupleName(T_IN)))
        s = s.addMap(m)
    })

    if (father == null)
      return List(s)
    return father.getSchedulePrefix(stmts) ++ List(s)
  }

  def accept[T](v : ScheduleNodeVisitor[T]) : T = {
    return v.visit(this)
  }

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = {
    return v.visit(this, param)
  }

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[DimNode]) {
      val oDimNode : DimNode = o.asInstanceOf[DimNode]
      var equal = this.isRoot == oDimNode.isRoot
      equal &&= this.domain.isEqual(oDimNode.domain)
      equal &&= this.sched.isEqual(oDimNode.sched)
      equal &&= this.child.equals(oDimNode.child)
      return equal
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = Isl.islUnionSetUniqueToString(this.domain).hashCode
    hash = hash * prime + Isl.islUnionMapUniqueToString(this.sched).hashCode()
    hash = hash * prime + this.child.hashCode
    return hash
  }

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    sb.append("DimNode(")
      .append(Isl.islUnionMapUniqueToString(this.sched))
      .append(");")
      .append(child)
    return sb.toString
  }
}

class LeafNode private () extends ScheduleNode {

  def this(domain : isl.UnionSet, coeffMatrDims : List[Int]) = {
    this()
    this.domain = domain
    this.coeffMatrDims = coeffMatrDims
  }

  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap] = {
    if (father == null)
      return List.empty
    return father.getSchedulePrefix(stmts)
  }

  def accept[T](v : ScheduleNodeVisitor[T]) : T = {
    return v.visit(this)
  }

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = {
    return v.visit(this, param)
  }

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[LeafNode]) {
      val oLeafNode : LeafNode = o.asInstanceOf[LeafNode]
      return this.domain.isEqual(oLeafNode.domain)
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = Isl.islUnionSetUniqueToString(domain).hashCode()
    return hash
  }

  override def toString() : String = "Leaf"
}

abstract class BandNode private () extends ScheduleNode {

  protected var child : ScheduleNode = null
  protected var scheds : List[(isl.UnionMap, Boolean)] = null

  def this(domain : isl.UnionSet, scheds : List[(isl.UnionMap, Boolean)],
    child : ScheduleNode, coeffMatrDims : List[Int]) = {
    this()
    this.domain = domain
    this.child = child
    this.scheds = scheds
    this.coeffMatrDims = coeffMatrDims
    child.setFather(this)
  }

  def getSchedulePrefix(stmts : Set[String]) : List[isl.UnionMap] = {
    val lList : List[isl.UnionMap] = scheds.map(_._1).map((m : isl.UnionMap) => {
      var dimSchedFiltered : isl.UnionMap = isl.UnionMap.empty(m.getSpace)
      m.foreachMap((mm : isl.Map) => {
        if (stmts.contains(mm.getTupleName(T_IN)))
          dimSchedFiltered = dimSchedFiltered.addMap(mm)
      })
      dimSchedFiltered
    })
    if (father == null)
      return lList
    return father.getSchedulePrefix(stmts) ++ lList
  }

  def getScheds : List[(isl.UnionMap, Boolean)] = scheds

  def getChild : ScheduleNode = child

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[BandNode]) {
      val oBandNode : BandNode = o.asInstanceOf[BandNode]
      if (this.scheds.size == oBandNode.scheds.size) {
        var equal : Boolean = this.domain.isEqual(oBandNode.domain)
        equal &&= this.scheds.zip(oBandNode.scheds).forall(t => t._1._1.isEqual(t._2._1) && (t._1._2 == t._2._2))
        return equal && this.child.equals(oBandNode.child)
      }
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = Isl.islUnionSetUniqueToString(this.domain).hashCode()
    hash = hash * prime + this.scheds.map(t => (Isl.islUnionMapUniqueToString(t._1), t._2)).hashCode()
    return hash
  }

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    val thisStr : String = scheds.map(t => {
      "(" + Isl.islUnionMapUniqueToString(t._1) + ", coincident: " + t._2 + ")"
    }).mkString("[", ";", "]")
    sb.append("BandNode")
      .append(thisStr)
      .append(";")
      .append(child)
    return sb.toString
  }
}

class SimpleBandNode(domain : isl.UnionSet, scheds : List[(isl.UnionMap, Boolean)],
  child : ScheduleNode, coeffMatrDims : List[Int]) extends BandNode(domain, scheds, child, coeffMatrDims) {

  def accept[T](v : ScheduleNodeVisitor[T]) : T = v.visit(this)

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = v.visit(this, param)
}

class BandNodeLoop private (domain : isl.UnionSet, scheds : List[(isl.UnionMap, Boolean)], child : ScheduleNode, coeffMatrDims : List[Int])
  extends BandNode(domain, scheds, child, coeffMatrDims) {
  // additional info
  private var loop : List[Map[String, Boolean]] = null

  def this(bn : BandNode, loop : List[Map[String, Boolean]], coeffMatrDims : List[Int]) = {
    this(bn.getDomain, bn.getScheds, bn.getChild, coeffMatrDims)
    this.loop = loop
    child.setFather(this)
  }

  def this(domain : isl.UnionSet, scheds : List[(isl.UnionMap, Boolean)],
    child : ScheduleNode, loop : List[Map[String, Boolean]], coeffMatrDims : List[Int]) = {
    this(domain, scheds, child, coeffMatrDims)
    this.loop = loop
    child.setFather(this)
  }

  def getLoop : List[Map[String, Boolean]] = loop

  def isInnerMostBandNodeWithLoop() : Boolean = {
    val withLoop = loop.map(f => f.values).flatten.fold(false)((a, b) => a || b)
    if (!withLoop) {
      return false
    }
    return this.getChild.accept(new InnermostLoopVisitor)
  }

  override def equals(o : Any) : Boolean = {
    if (o.isInstanceOf[BandNodeLoop]) {
      val oBandNodeLoop : BandNodeLoop = o.asInstanceOf[BandNodeLoop]
      var equal : Boolean = this.loop.equals(oBandNodeLoop.loop)
      equal &&= super.equals(o)
      return equal
    }
    return false
  }

  override def hashCode() : Int = {
    val prime : Int = 13
    var hash : Int = super.hashCode()
    hash = hash * 13 + loop.hashCode()
    return hash
  }

  override def toString() : String = {
    val sb : StringBuilder = StringBuilder.newBuilder
    val thisStr : String = scheds.zip(loop).map(t => {
      val sched : isl.UnionMap = t._1._1
      val coincident : Boolean = t._1._2
      val loopInfo : Map[String, Boolean] = t._2
      "(" + Isl.islUnionMapUniqueToString(sched) + ", coincident: " + coincident + ", loopInfo: " + loopInfo + ")"
    }).mkString("[, ", ";", "]")
    sb.append("BandNodeLoop")
      .append(thisStr)
      .append(";")
      .append(child)
    return sb.toString
  }

  def accept[T](v : ScheduleNodeVisitor[T]) : T = v.visit(this)

  def accept[T, P](v : ParamScheduleNodeVisitor[T, P], param : P) : T = v.visit(this, param)
}

/**
  * Must be called on the child of a BandNodeLoop.
  *
  * Returns the info whether their is another BandNode in the subtree which generates a loop.
  */
private class InnermostLoopVisitor extends ScheduleNodeVisitor[Boolean] {

  def visit(b : SimpleBandNode) : Boolean = {
    throw new IllegalArgumentException("SimpleBandNode not allowed, no loop information")
  }

  def visit(b : DimNode) : Boolean = {
    throw new IllegalArgumentException("DimNode not allowed, no loop information")
  }

  def visit(b : SeqNode) : Boolean = {
    return b.getChildren.map(f => f._2.accept(this)).fold(true)((a, b) => a && b)
  }

  def visit(b : SetNode) : Boolean = {
    return b.getChildren.map(f => f._2.accept(this)).fold(true)((a, b) => a && b)
  }

  def visit(b : LeafNode) : Boolean = {
    return true
  }

  override def visit(b : BandNodeLoop) : Boolean = {
    val withLoop = b.getLoop.map(f => f.values).flatten.fold(false)((a, b) => a || b)
    return !withLoop
  }
}

trait ScheduleNodeVisitor[T] {
  def visit(b : SimpleBandNode) : T;
  def visit(b : BandNodeLoop) : T;
  def visit(b : DimNode) : T;
  def visit(b : SeqNode) : T;
  def visit(b : SetNode) : T;
  def visit(b : LeafNode) : T;
}

trait ParamScheduleNodeVisitor[T, P] {
  def visit(b : SimpleBandNode, param : P) : T;
  def visit(b : BandNodeLoop, param : P) : T;
  def visit(b : DimNode, param : P) : T;
  def visit(b : SeqNode, param : P) : T;
  def visit(b : SetNode, param : P) : T;
  def visit(b : LeafNode, param : P) : T;
}

trait ScheduleNodeVisitorLeaveNDegUnchanged extends ScheduleNodeVisitor[ScheduleNode] {

  def visit(n : SeqNode) : ScheduleNode = {
    val newChildren : List[(Set[String], ScheduleNode)] = n.getChildren.map((t : (Set[String], ScheduleNode)) => {
      (t._1, t._2.accept(this))
    })
    return new SeqNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }

  def visit(n : SetNode) : ScheduleNode = {
    val newChildren : Set[(Set[String], ScheduleNode)] = n.getChildren.map((t : (Set[String], ScheduleNode)) => {
      (t._1, t._2.accept(this))
    })
    return new SetNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }
}

trait ParamScheduleNodeVisitorLeaveNDegUnchanged[P] extends ParamScheduleNodeVisitor[ScheduleNode, P] {

  def visit(n : SeqNode, p : P) : ScheduleNode = {
    val newChildren : List[(Set[String], ScheduleNode)] = n.getChildren.map((t : (Set[String], ScheduleNode)) => {
      (t._1, t._2.accept(this, p))
    })
    return new SeqNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }

  def visit(n : SetNode, p : P) : ScheduleNode = {
    val newChildren : Set[(Set[String], ScheduleNode)] = n.getChildren.map((t : (Set[String], ScheduleNode)) => {
      (t._1, t._2.accept(this, p))
    })
    return new SetNode(n.getDomain, newChildren, n.getCoeffMatrDims)
  }
}