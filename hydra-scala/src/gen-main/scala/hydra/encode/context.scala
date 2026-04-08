package hydra.encode.context

import hydra.context.*

import hydra.core.*

def context(x: hydra.context.Context): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.context.Context", Seq(hydra.core.Field("trace", hydra.core.Term.list(hydra.lib.lists.map[scala.Predef.String,
     hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.trace))),
     hydra.core.Field("messages", hydra.core.Term.list(hydra.lib.lists.map[scala.Predef.String, hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x.messages))),
     hydra.core.Field("other", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name, hydra.core.Term,
     hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(x.other))))))

def inContext[T0](e: (T0 => hydra.core.Term))(x: hydra.context.InContext[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.context.InContext", Seq(hydra.core.Field("object", e(x.`object`)),
     hydra.core.Field("context", hydra.encode.context.context(x.context)))))
