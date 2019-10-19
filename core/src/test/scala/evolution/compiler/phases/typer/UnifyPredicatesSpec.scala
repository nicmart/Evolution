package evolution.compiler.phases.typer

import scala.util.Random
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typer.UnifyPredicates
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.model.Substitution
import evolution.logging.NoOpLogger
import evolution.compiler.LanguageSpec

class UnifyPredicatesSpec extends LanguageSpec {

  "predicates unification" - {
    "should succeed with an empty substitution if there are no predicates" in {
      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, Nil)
      subst shouldBe Right(Substitution.empty)
    }

    "should succeed with an empty substitution if there is a single predicate that is the same as an instance" in {
      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, TypingConfig.instancesPredicates.take(1))
      subst shouldBe Right(Substitution.empty)
    }

    "should succeed with a simple substitution if there is a single predicate with vars and a matching instance" in {
      val predicates = List(Predicate("Num", List(TypeT.Var("X"))))
      val instances = List(Predicate("Num", List(TypeT.Double)))
      val subst = predicatesUnifier.unify(instances, predicates)

      subst shouldBe Right(Substitution("X" -> TypeT.Double))
    }

    "should succeed with higher-horder types in predicates" in {
      val predicates = List(Predicate("Num", List(TypeT.Evo(TypeT.Var("X")))))
      val instances = List(Predicate("Num", List(TypeT.Evo(TypeT.Double))))
      val subst = predicatesUnifier.unify(instances, predicates)

      subst shouldBe Right(Substitution("X" -> TypeT.Double))
    }

    "should do more complex unifications" in {
      val predicates = List(
        Predicate("Num", List(TypeT.Var("Y"))),
        Predicate("Both", List(TypeT.Var("X"), TypeT.Var("Y")))
      )

      val instances = List(
        Predicate("Num", List(TypeT.Point)),
        Predicate("Num", List(TypeT.Integer)),
        Predicate("Both", List(TypeT.Double, TypeT.Double)),
        Predicate("Both", List(TypeT.Integer, TypeT.Integer))
      )
      val subst = predicatesUnifier.unify(instances, predicates).unsafeEvaluate

      subst.substitute[Type](TypeT.Var("X")) shouldBe TypeT.Integer
      subst.substitute[Type](TypeT.Var("Y")) shouldBe TypeT.Integer
    }

    // This is mostly to test perfomance of predicates unification
    "should uniffy predicates of @(point(0, 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8))" in {
      val predicates = List(
        Predicate("Num", List(TypeT.Double)),
        Predicate("Mult", List(TypeT.Var("T1"), TypeT.Var("T7"), TypeT.Double)),
        Predicate("Num", List(TypeT.Var("T1"))),
        Predicate("Mult", List(TypeT.Var("T2"), TypeT.Var("T6"), TypeT.Var("T7"))),
        Predicate("Num", List(TypeT.Var("T2"))),
        Predicate("Mult", List(TypeT.Var("T8"), TypeT.Var("T5"), TypeT.Var("T6"))),
        Predicate("Num", List(TypeT.Var("T8"))),
        Predicate("Mult", List(TypeT.Var("T3"), TypeT.Var("T4"), TypeT.Var("T5"))),
        Predicate("Num", List(TypeT.Var("T3"))),
        Predicate("Mult", List(TypeT.Var("T9"), TypeT.Var("T10"), TypeT.Var("T4"))),
        Predicate("Num", List(TypeT.Var("T9"))),
        Predicate("Mult", List(TypeT.Var("T11"), TypeT.Var("T12"), TypeT.Var("T0"))),
        Predicate("Num", List(TypeT.Var("T11"))),
        Predicate("Mult", List(TypeT.Var("T13"), TypeT.Var("T14"), TypeT.Var("T12"))),
        Predicate("Num", List(TypeT.Var("T13"))),
        Predicate("Num", List(TypeT.Var("T14")))
      )

      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, Random.shuffle(predicates)).unsafeEvaluate

      subst.substitute[Type](TypeT.Var("T4")) shouldBe TypeT.Double
    }

    "should unify predicates of a = 1 in x = a * a * a * a  *  point(0, 0) in x" in {
      def predicate(n: Int) = Predicate("Mult", List(TypeT.Var("T0"), TypeT.Var(s"T$n"), TypeT.Var(s"T${n + 1}")))

      val predicates = List(
        Predicate("Num", List(TypeT.Var("T0"))),
        Predicate("Mult", List(TypeT.Var("T0"), TypeT.Point, TypeT.Var("T1")))
      ) ++ (1 to 40).map(predicate)

      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, predicates).unsafeEvaluate

      subst.substitute[Type](TypeT.Var("T0")) shouldBe TypeT.Double
    }

    "should fail if there are no instances and there is at least one predicate" in {
      val predicates = List(Predicate("Num", List(TypeT.Var("X"))))
      val result = predicatesUnifier.unify(Nil, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the only instance does not match the typeclass of the only predicate" in {
      val instances = List(Predicate("Num", List(TypeT.Integer)))
      val predicates = List(Predicate("Whatever", List(TypeT.Integer)))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the only instance does not match the types of the only predicate" in {
      val instances = List(Predicate("Num", List(TypeT.Integer)))
      val predicates = List(Predicate("Num", List(TypeT.Double)))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the the matching instance leads to incompativle assignments" in {
      val instances = List(Predicate("Bi", List(TypeT.Integer, TypeT.Double)))
      val predicates = List(Predicate("Bi", List(TypeT.Var("x"), TypeT.Var("x"))))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }
  }

  lazy val predicatesUnifier = new UnifyPredicates(NoOpLogger)
}
