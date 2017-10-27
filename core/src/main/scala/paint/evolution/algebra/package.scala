package paint.evolution

package object algebra {
  trait FullAlgebra[Evo[+_]]
    extends EvolutionAlgebra[Evo]
      with NumericEvolutionAlgebra[Evo]
      with DistributionEvolutionAlgebra[Evo]
      with SemigroupEvolutionAlgebra[Evo]
      with MotionEvolutionAlgebra[Evo]
      with PointEvolutionAlgebra[Evo]
}
