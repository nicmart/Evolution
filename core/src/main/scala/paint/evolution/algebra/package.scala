package paint.evolution

package object algebra {
  type MaterializedAlgebra[Algebra[_[_]], Evo[+ _], W] =
    Algebra[Evo] with EvolutionMaterialization[Evo, W]

  trait FullAlgebra[Evo[+_]]
    extends EvolutionAlgebra[Evo]
      with NumericEvolutionAlgebra[Evo]
      with DistributionEvolutionAlgebra[Evo]
      with SemigroupEvolutionAlgebra[Evo]
      with MotionEvolutionAlgebra[Evo]
      with PointEvolutionAlgebra[Evo]

  trait MaterializableFullAlgebra[Evo[+_], W]
    extends FullAlgebra[Evo]
      with EvolutionMaterialization[Evo, W]
}
