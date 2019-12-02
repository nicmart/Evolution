package evolution.compiler.term

// F will be String for JsMaterialization, Register => Any for EvalMaterialization
trait TermMaterializer[F[+_]] {
  def materialize(term: Term): F[Any]
}
