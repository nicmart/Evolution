package evolution.compiler.phases.typer

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.model.Substitution
import evolution.compiler.types.TypeClasses._
import evolution.compiler.types._
import evolution.logging.NoOpLogger

import scala.util.Random

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
      val predicates = List(Predicate("Num", List(Type.Var("X"))))
      val instances = List(Predicate("Num", List(Type.Double)))
      val subst = predicatesUnifier.unify(instances, predicates)

      subst shouldBe Right(Substitution("X" -> Type.Double))
    }

    "should succeed with higher-horder types in predicates" in {
      val predicates = List(Predicate("Num", List(Type.Evo(Type.Var("X")))))
      val instances = List(Predicate("Num", List(Type.Evo(Type.Double))))
      val subst = predicatesUnifier.unify(instances, predicates)

      subst shouldBe Right(Substitution("X" -> Type.Double))
    }

    "should do more complex unifications" in {
      val predicates = List(
        Predicate("Num", List(Type.Var("Y"))),
        Predicate("Both", List(Type.Var("X"), Type.Var("Y")))
      )

      val instances = List(
        Predicate("Num", List(Type.Point)),
        Predicate("Num", List(Type.Integer)),
        Predicate("Both", List(Type.Double, Type.Double)),
        Predicate("Both", List(Type.Integer, Type.Integer))
      )
      val subst = predicatesUnifier.unify(instances, predicates).unsafeRight

      subst.substitute[Type](Type.Var("X")) shouldBe Type.Integer
      subst.substitute[Type](Type.Var("Y")) shouldBe Type.Integer
    }

    // This is mostly to test perfomance of predicates unification
    "should uniffy predicates of @(point(0, 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8))" in {
      val predicates = List(
        Predicate("Num", List(Type.Double)),
        Predicate("Mult", List(Type.Var("T1"), Type.Var("T7"), Type.Double)),
        Predicate("Num", List(Type.Var("T1"))),
        Predicate("Mult", List(Type.Var("T2"), Type.Var("T6"), Type.Var("T7"))),
        Predicate("Num", List(Type.Var("T2"))),
        Predicate("Mult", List(Type.Var("T8"), Type.Var("T5"), Type.Var("T6"))),
        Predicate("Num", List(Type.Var("T8"))),
        Predicate("Mult", List(Type.Var("T3"), Type.Var("T4"), Type.Var("T5"))),
        Predicate("Num", List(Type.Var("T3"))),
        Predicate("Mult", List(Type.Var("T9"), Type.Var("T10"), Type.Var("T4"))),
        Predicate("Num", List(Type.Var("T9"))),
        Predicate("Mult", List(Type.Var("T11"), Type.Var("T12"), Type.Var("T0"))),
        Predicate("Num", List(Type.Var("T11"))),
        Predicate("Mult", List(Type.Var("T13"), Type.Var("T14"), Type.Var("T12"))),
        Predicate("Num", List(Type.Var("T13"))),
        Predicate("Num", List(Type.Var("T14")))
      )

      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, Random.shuffle(predicates)).unsafeRight

      subst.substitute[Type](Type.Var("T4")) shouldBe Type.Double
    }

    "should unify predicates of a = 1 in x = a * a * a * a  *  point(0, 0) in x" in {
      def predicate(n: Int) = Predicate("Mult", List(Type.Var("T0"), Type.Var(s"T$n"), Type.Var(s"T${n + 1}")))

      val predicates = List(
        Predicate("Num", List(Type.Var("T0"))),
        Predicate("Mult", List(Type.Var("T0"), Type.Point, Type.Var("T1")))
      ) ++ (1 to 40).map(predicate)

      val subst = predicatesUnifier.unify(TypingConfig.instancesPredicates, predicates).unsafeRight

      subst.substitute[Type](Type.Var("T0")) shouldBe Type.Double
    }

    "should not do stupid things" in {
      pending
      val predicates = List(
        Predicate("Mult", List(Type.Var("A"), Type.Var("B"), Type.Var("C")))
      )
      val instances = List(
        Predicate("Mult", List(Type.Double, Type.Double, Type.Double)),
        Predicate("Mult", List(Type.Double, Type.Point, Type.Point)),
        Predicate("Mult", List(Type.Point, Type.Double, Type.Point))
      )

      val subst = predicatesUnifier.unify(instances, predicates).unsafeRight
      subst.substitute(predicates) shouldBe predicates
    }

    "should fail if there are no instances and there is at least one predicate" in {
      val predicates = List(Predicate("Num", List(Type.Var("X"))))
      val result = predicatesUnifier.unify(Nil, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the only instance does not match the typeclass of the only predicate" in {
      val instances = List(Predicate("Num", List(Type.Integer)))
      val predicates = List(Predicate("Whatever", List(Type.Integer)))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the only instance does not match the types of the only predicate" in {
      val instances = List(Predicate("Num", List(Type.Integer)))
      val predicates = List(Predicate("Num", List(Type.Double)))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }

    "should fail if the the matching instance leads to incompatible assignments" in {
      val instances = List(Predicate("Bi", List(Type.Integer, Type.Double)))
      val predicates = List(Predicate("Bi", List(Type.Var("x"), Type.Var("x"))))
      val result = predicatesUnifier.unify(instances, predicates)

      result.isLeft shouldBe true
    }
  }

  lazy val predicatesUnifier = new UnifyPredicates(NoOpLogger)
}
