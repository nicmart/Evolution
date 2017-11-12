package evolution.app.model.definition.state

import evolution.app.model.definition.DrawingDefinition

trait DrawingState[Seed, T] {
  val definition: DrawingDefinition[T]
  val seed: Seed
  val config: definition.Config
}