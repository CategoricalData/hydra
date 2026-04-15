package hydra.eval.lib.logic

import hydra.core.*

def and[T0, T1, T2](cx: T0)(g: T1)(a: hydra.core.Term)(b: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     a)), b)), hydra.core.Term.literal(hydra.core.Literal.boolean(false)))))

def not[T0, T1, T2](cx: T0)(g: T1)(a: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     a)), hydra.core.Term.literal(hydra.core.Literal.boolean(false)))), hydra.core.Term.literal(hydra.core.Literal.boolean(true)))))

def or[T0, T1, T2](cx: T0)(g: T1)(a: hydra.core.Term)(b: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     a)), hydra.core.Term.literal(hydra.core.Literal.boolean(true)))), b)))
