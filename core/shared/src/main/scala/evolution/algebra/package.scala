package evolution

import evolution.algebra._

package object algebra {
  trait FullAlgebra[Evo[+ _]]
      extends LegacyEvolutionAlgebra[Evo]
      with NumericEvolutionAlgebra[Evo]
      with DistributionEvolutionAlgebra[Evo]
      with SemigroupEvolutionAlgebra[Evo]
      with MotionEvolutionAlgebra[Evo]
      with PointEvolutionAlgebra[Evo]
}
