package hydra.encode.coders

import hydra.coders.*

import hydra.core.*

def coderDirection(v1: hydra.coders.CoderDirection): hydra.core.Term =
  v1 match
  case hydra.coders.CoderDirection.encode => hydra.core.Term.inject(hydra.core.Injection("hydra.coders.CoderDirection",
     hydra.core.Field("encode", hydra.core.Term.unit)))
  case hydra.coders.CoderDirection.decode => hydra.core.Term.inject(hydra.core.Injection("hydra.coders.CoderDirection",
     hydra.core.Field("decode", hydra.core.Term.unit)))

def languageName(x: hydra.coders.LanguageName): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.coders.LanguageName", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def traversalOrder(v1: hydra.coders.TraversalOrder): hydra.core.Term =
  v1 match
  case hydra.coders.TraversalOrder.pre => hydra.core.Term.inject(hydra.core.Injection("hydra.coders.TraversalOrder",
     hydra.core.Field("pre", hydra.core.Term.unit)))
  case hydra.coders.TraversalOrder.post => hydra.core.Term.inject(hydra.core.Injection("hydra.coders.TraversalOrder",
     hydra.core.Field("post", hydra.core.Term.unit)))
