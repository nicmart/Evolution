package paint.evolution.generator

import paint.evolution.Evolution

trait EvolutionGenerator[T] {
    type Context
    def evolution(context: Context): Evolution[T]
}

object EvolutionGenerator {
    type Aux[T, C] = EvolutionGenerator[T] { type Context = C }
    def apply[T, C](f: C => Evolution[T]): Aux[T, C] = new EvolutionGenerator[T] {
        type Context = C
        def evolution(context: Context): Evolution[T] = f(context)
    }
}
