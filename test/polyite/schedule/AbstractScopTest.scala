package polyite.schedule

import java.util.Properties
import polyite.config.Config
import org.junit.Before
import polyite.ScopInfo
import polyite.export.JSCOPInterface
import java.util.logging.Logger
import polyite.AbstractTest
import isl.Isl

object AbstractScopTest {

  private def checkIsValidSched(deps : Set[Dependence], sched : isl.UnionMap) {
    val schedDims : List[isl.UnionMap] = Isl.splitMultiDimUnionMap(sched)
    val uncarriedDeps : Set[Dependence] = schedDims.zipWithIndex.foldLeft(deps)((remDeps : Set[Dependence], dimSched : (isl.UnionMap, Int)) => {
      remDeps.view.foreach((dep : Dependence) => {
        assert(!ScheduleUtils.getDirectionOfDep(dimSched._1, dep).isNegative, f"dependence ${dep} is violated by dimension ${dimSched._2}")
      })
      remDeps -- ScheduleUtils.getDepsCarriedBySchedule(dimSched._1, remDeps)
    })
    assert(uncarriedDeps.isEmpty, f"There are uncarried dependences:\n${uncarriedDeps.mkString("\n")}")
  }
}

abstract class AbstractScopTest extends AbstractTest {

  protected val myLogger = Logger.getLogger("")

  protected var scop : ScopInfo = null
  protected var deps : Set[Dependence] = null
  protected var domInfo : DomainCoeffInfo = null
  protected var conf : Config = null
  protected var scopStr : String = null

  @Before
  def prepare() {
    scop = JSCOPInterface.parseJSCOP(scopStr) match {
      case None    => throw new RuntimeException()
      case Some(s) => s
    }

    val t : (Set[Dependence], DomainCoeffInfo) = ScheduleSpaceUtils.calcDepsAndDomInfo(scop)
    deps = t._1
    domInfo = t._2

    conf = createTestConfig() match {
      case None    => throw new RuntimeException()
      case Some(c) => c
    }
    runFurtherPreparation()
  }

  def runFurtherPreparation()
}