package hydra.encode.typing

import hydra.core.*

import hydra.typing.*

def functionStructure[T0](env: (T0 => hydra.core.Term))(x: hydra.typing.FunctionStructure[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.typing.FunctionStructure", Seq(hydra.core.Field("typeParams",
     hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.typeParams))),
     hydra.core.Field("params", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Name, hydra.core.Term](hydra.encode.core.name)(x.params))),
     hydra.core.Field("bindings", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Binding, hydra.core.Term](hydra.encode.core.binding)(x.bindings))),
     hydra.core.Field("body", hydra.encode.core.term(x.body)), hydra.core.Field("domains", hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Type,
     hydra.core.Term](hydra.encode.core.`type`)(x.domains))), hydra.core.Field("codomain", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.core.Type,
     hydra.core.Term](hydra.encode.core.`type`)(x.codomain))), hydra.core.Field("environment", env(x.environment)))))

def inferenceResult(x: hydra.typing.InferenceResult): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.typing.InferenceResult", Seq(hydra.core.Field("term",
     hydra.encode.core.term(x.term)), hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)), hydra.core.Field("subst",
     hydra.encode.typing.typeSubst(x.subst)), hydra.core.Field("classConstraints", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name,
     hydra.core.Term, hydra.core.TypeVariableMetadata, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.typeVariableMetadata)(x.classConstraints))),
     hydra.core.Field("context", hydra.encode.context.context(x.context)))))

def termSubst(x: hydra.typing.TermSubst): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.typing.TermSubst", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name,
     hydra.core.Term, hydra.core.Term, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.term)(x))))

def typeConstraint(x: hydra.typing.TypeConstraint): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.typing.TypeConstraint", Seq(hydra.core.Field("left",
     hydra.encode.core.`type`(x.left)), hydra.core.Field("right", hydra.encode.core.`type`(x.right)),
     hydra.core.Field("comment", hydra.core.Term.literal(hydra.core.Literal.string(x.comment))))))

def typeSubst(x: hydra.typing.TypeSubst): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.typing.TypeSubst", hydra.core.Term.map(hydra.lib.maps.bimap[hydra.core.Name,
     hydra.core.Term, hydra.core.Type, hydra.core.Term](hydra.encode.core.name)(hydra.encode.core.`type`)(x))))
