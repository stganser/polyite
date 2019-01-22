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
 * @param constraints The constraint representation of the set as an isl set. Note, that the given sets may relate to
 * another isl context than the global isl context of Polyite. Therefore, {@code constraints} must not be used together
 * with the global isl context.
 */
case class ConstraintRepr(constraints : isl.Set) extends Polyhedron