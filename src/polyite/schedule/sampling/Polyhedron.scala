package polyite.schedule.sampling

import polyite.schedule.sampling.ChernikovaSamplingStrategy.GeneratorsRat

/**
 * Representation of a polyhedron.
 */
trait Polyhedron {

}

/**
 * Representation of a polyhedron by Chernikovas generators.
 */
case class GeometricRepr(generators : GeneratorsRat) extends Polyhedron

/**
 * Constraint representation of polyhedra.
 */
case class ConstraintRepr(constraints : isl.Set) extends Polyhedron