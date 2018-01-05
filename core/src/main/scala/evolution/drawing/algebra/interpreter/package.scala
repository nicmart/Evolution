package evolution.drawing.algebra

import evolution.algebra.Evolution

package object interpreter {
  type ConstString[-E, +A] = String
  type CtxString[-E, +A] = E => String
  type CtxEvolution[-E, +A] = E => Evolution[A]
}
