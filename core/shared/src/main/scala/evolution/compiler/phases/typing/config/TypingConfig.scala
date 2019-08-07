package evolution.compiler.phases.typing.config

import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{ Type, TypeBinding, TypeBindings }

object TypingConfig {
  val constantQualifiedTypes: TypeBindings =
    new TypeBindings(
      Constant.values
        .map(constant => constant.entryName -> TypeBinding.Predefined(constant.entryName, constant.qualifiedType))
        .toMap
    )

  /**
   * TODO A lot of coupling between this, All the instances, and Typeclass extraction in Types Module
   */
  import Type._
  val instances: List[Predicate] = List(
    Predicate("Num", List(Dbl)),
    Predicate("Num", List(Integer)),
    Predicate("Mult", List(Dbl, Dbl, Dbl)),
    Predicate("Mult", List(Dbl, Point, Point)),
    Predicate("Mult", List(Point, Dbl, Point)),
    Predicate("Mult", List(Integer, Integer, Integer)),
    Predicate("Mult", List(Integer, Dbl, Dbl)),
    Predicate("Mult", List(Dbl, Integer, Dbl)),
    Predicate("Mult", List(Integer, Point, Point)),
    Predicate("Mult", List(Dbl, Evo(Dbl), Evo(Dbl))),
    Predicate("Mult", List(Evo(Dbl), Dbl, Evo(Dbl))),
    Predicate("Mult", List(Dbl, Evo(Point), Evo(Point))),
    Predicate("Mult", List(Evo(Point), Dbl, Evo(Point))),
    Predicate("Mult", List(Evo(Dbl), Evo(Dbl), Evo(Dbl))),
    Predicate("Mult", List(Evo(Point), Evo(Dbl), Evo(Point))),
    Predicate("Mult", List(Evo(Dbl), Evo(Point), Evo(Point))),
    Predicate("Add", List(Dbl, Dbl, Dbl)),
    Predicate("Add", List(Integer, Integer, Integer)),
    Predicate("Add", List(Integer, Dbl, Dbl)),
    Predicate("Add", List(Dbl, Integer, Dbl)),
    Predicate("Add", List(Point, Point, Point)),
    Predicate("Add", List(Evo(Dbl), Evo(Dbl), Evo(Dbl))),
    Predicate("Add", List(Evo(Point), Evo(Point), Evo(Point))),
    Predicate("Invertible", List(Integer)),
    Predicate("Invertible", List(Dbl)),
    Predicate("Invertible", List(Point)),
    Predicate("Invertible", List(Evo(Dbl))),
    Predicate("Invertible", List(Evo(Point)))
  )
}
