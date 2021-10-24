package evolution.compiler.phases.typer

import evolution.compiler.phases.typer.model.{Assumptions, Substitution}
import evolution.compiler.types.Type

private[typer] final case class InferenceState(
    private val count: Int,
    substitution: Substitution,
    assumptions: Assumptions
):
  def currentTypeVarname: String = s"T$count"
  def currentTypeVar: Type = Type.Var(currentTypeVarname)
  def withNewTypeVar: InferenceState = copy(count = count + 1)

  def withSubstitution(substitution: Substitution): InferenceState =
    copy(substitution = substitution)

  def withAssumptions(assumptions: Assumptions): InferenceState =
    copy(assumptions = assumptions)

private[typer] object InferenceState:
  def empty: InferenceState =
    new InferenceState(0, Substitution.empty, Assumptions.empty)
