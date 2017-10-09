package paint.evolution

package object algebra {
  type MaterializedAlgebra[Algebra[_[_]], Evo[+ _], W] =
    Algebra[Evo] with EvolutionMaterialization[Evo, W]
}
