package polyite.fitness.classifier

import org.junit.Test
import org.junit.Assert._
import polyite.fitness.scikit_learn.Classifier
import polyite.fitness.Feature
import polyite.fitness.FeatureVect
import polyite.schedule.Schedule
import polyite.fitness.MemAccessPattern
import java.io.File
import polyite.fitness.DataLocality
import polyite.fitness.NumLeafs
import polyite.fitness.NumSeqAndSetNodes
import polyite.fitness.ParallelLoops
import polyite.fitness.SparsityIterCoeffs
import polyite.fitness.TilableBands
import polyite.fitness.TreeDepth
import polyite.fitness.Prediction

class TestClassifier {

  val testLearnFiles : List[File] = List(getClass.getResource("test_learning_set_1.csv").getPath,
    getClass.getResource("test_learning_set_2.csv").getPath).map(new File(_))
    
  val features : List[Feature] = Feature.features.filterNot(x => x == MemAccessPattern)

  @Test
  def testLearnCART() {
    val c = new Classifier(features, testLearnFiles, 14, Classifier.LearningAlgorithms.CART, None, None)
    c.destroy()
  }

  @Test
  def testLearnRandomForest() {
    val c = new Classifier(features, testLearnFiles, 14, Classifier.LearningAlgorithms.RANDOM_FOREST,
      Some(new Classifier.RandomForestConfig(100, 7)), None)
    c.destroy()
  }

  @Test
  def testPredictCART() {
    val c : Classifier = new Classifier(features, testLearnFiles, 14, Classifier.LearningAlgorithms.CART, None, None)
    var fVect = new FeatureVect(features.map { f => (f, 0.5) } toMap)
    val predClass = c.predict(fVect)
    println(predClass)
    c.exportTree(new File("/tmp/tree.dot"))
    c.destroy()
  }

  @Test
  def testPredictRandomForestNegative() {
    val c : Classifier = new Classifier(features, testLearnFiles, 14, Classifier.LearningAlgorithms.RANDOM_FOREST,
      Some(new Classifier.RandomForestConfig(100, 7)), None)
    var fVect = new FeatureVect(Map((DataLocality, 0.484425132446411), (NumLeafs, 1), (NumSeqAndSetNodes, 1), (ParallelLoops, 0.583333333333333), (SparsityIterCoeffs, 0.461538461538462), (TilableBands, 0.583333333333333), (TreeDepth, 2/3)))
    val predClass = c.predict(fVect)
    println(predClass)
    assertTrue(predClass.pClass.get == Prediction.PerfClass.BAD)
    c.destroy()
  }
  
  @Test
  def testPredictRandomForestPositive() {
    val c : Classifier = new Classifier(features, testLearnFiles, 14, Classifier.LearningAlgorithms.RANDOM_FOREST,
      Some(new Classifier.RandomForestConfig(100, 7)), None)
    var fVect = new FeatureVect(Map((DataLocality, 0.625), (NumLeafs, 1), (NumSeqAndSetNodes, 1), (ParallelLoops, 1), (SparsityIterCoeffs, 2.0/3.0), (TilableBands, 0.833333), (TreeDepth, 2/3)))
    val predClass = c.predict(fVect)
    println(predClass)
    assertTrue(predClass.pClass.get == Prediction.PerfClass.GOOD)
    c.destroy()
  }
}