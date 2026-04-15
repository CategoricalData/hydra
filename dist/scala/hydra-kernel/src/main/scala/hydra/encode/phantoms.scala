package hydra.encode.phantoms

import hydra.core.*

import hydra.phantoms.*

def tBinding[T0, T1](a: T0)(x: hydra.phantoms.TBinding[T1]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.phantoms.TBinding", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("term", hydra.encode.phantoms.tTerm(a)(x.term)))))

def tTerm[T0, T1](a: T0)(x: hydra.phantoms.TTerm[T1]): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.phantoms.TTerm", hydra.encode.core.term(x)))

def tTermDefinition[T0, T1](a: T0)(x: hydra.phantoms.TTermDefinition[T1]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.phantoms.TTermDefinition", Seq(hydra.core.Field("name",
     hydra.encode.core.name(x.name)), hydra.core.Field("term", hydra.encode.phantoms.tTerm(a)(x.term)))))
