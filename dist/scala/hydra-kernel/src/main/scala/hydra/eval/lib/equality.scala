package hydra.eval.lib.equality

import hydra.core.*

def identity[T0, T1, T2, T3](cx: T0)(g: T1)(x: T2): Either[T3, T2] = Right(x)

def max[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term)(y: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.gte"),
     x)), y)))), x)), y)))

def min[T0, T1, T2](cx: T0)(g: T1)(x: hydra.core.Term)(y: hydra.core.Term): Either[T2, hydra.core.Term] =
  Right(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.logic.ifElse"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.equality.lte"),
     x)), y)))), x)), y)))
