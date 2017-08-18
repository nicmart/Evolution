package paint.evolution.generator

import paint.evolution.Evolution

trait EvolutionGenerator[T, Config] {
    def evolution(config: Config): Evolution[T]
}

object EvolutionGenerator {
    def apply[T, S](f: S => Evolution[T]): EvolutionGenerator[T, S] = new EvolutionGenerator[T, S] {
        def evolution(config: S): Evolution[T] = f(config)
    }
}
