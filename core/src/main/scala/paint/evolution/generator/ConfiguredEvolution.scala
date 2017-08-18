package paint.evolution.generator

import paint.evolution.Evolution

final case class ConfiguredEvolution[T, Config](generator: Config => Evolution[T], config: Config) {
    def evolution: Evolution[T] = generator(config)
    def withConfig(config: Config): ConfiguredEvolution[T, Config] =
        copy(config = config)
}
