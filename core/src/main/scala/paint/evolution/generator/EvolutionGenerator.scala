package paint.evolution.generator

import paint.evolution.Evolution

trait EvolutionGenerator[T, Settings] {
    def evolution(settings: Settings): Evolution[T]
}

object EvolutionGenerator {
    def apply[T, S](f: S => Evolution[T]): EvolutionGenerator[T, S] = new EvolutionGenerator[T, S] {
        def evolution(settings: S): Evolution[T] = f(settings)
    }
}
