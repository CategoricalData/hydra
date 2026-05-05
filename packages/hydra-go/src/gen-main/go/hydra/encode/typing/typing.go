// Note: this is an automatically generated file. Do not edit.

package encodetyping

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  encodecontext "hydra.dev/hydra/encode/context"
  encodecore "hydra.dev/hydra/encode/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libmaybes "hydra.dev/hydra/lib/maybes"
  "hydra.dev/hydra/typing"
)

func FunctionStructure[T0 any] (env func(T0) core.Term, x typing.FunctionStructure[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.typing.FunctionStructure"), Fields: []any{core.Field{Name: core.Name("typeParams"), Term: core.TermList{Value: liblists.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(typing.FunctionStructure[T0]).TypeParams
  }(x)).([]any)}}, core.Field{Name: core.Name("params"), Term: core.TermList{Value: liblists.Map(encodecore.Name).(func(any) any)(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Params
  }(x)).([]any)}}, core.Field{Name: core.Name("bindings"), Term: core.TermList{Value: liblists.Map(encodecore.Binding).(func(any) any)(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Bindings
  }(x)).([]any)}}, core.Field{Name: core.Name("body"), Term: encodecore.Term(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Body
  }(x).(core.Term))}, core.Field{Name: core.Name("domains"), Term: core.TermList{Value: liblists.Map(encodecore.Type_).(func(any) any)(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Domains
  }(x)).([]any)}}, core.Field{Name: core.Name("codomain"), Term: core.TermMaybe{Value: libmaybes.Map(encodecore.Type_).(func(any) any)(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Codomain
  }(x))}}, core.Field{Name: core.Name("environment"), Term: env(func (v any) any {
    return v.(typing.FunctionStructure[T0]).Environment
  }(x).(T0))}}}}
}

func InferenceResult (x typing.InferenceResult) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.typing.InferenceResult"), Fields: []any{core.Field{Name: core.Name("term"), Term: encodecore.Term(func (v any) any {
    return v.(typing.InferenceResult).Term
  }(x).(core.Term))}, core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(typing.InferenceResult).Type_
  }(x).(core.Type))}, core.Field{Name: core.Name("subst"), Term: TypeSubst(func (v any) any {
    return v.(typing.InferenceResult).Subst
  }(x).(typing.TypeSubst))}, core.Field{Name: core.Name("classConstraints"), Term: core.TermMap_{Value: libmaps.Bimap(encodecore.Name).(func(any) any)(encodecore.TypeVariableMetadata).(func(any) any)(func (v any) any {
    return v.(typing.InferenceResult).ClassConstraints
  }(x)).([]any)}}, core.Field{Name: core.Name("context"), Term: encodecontext.Context(func (v any) any {
    return v.(typing.InferenceResult).Context
  }(x).(context.Context))}}}}
}

func TermSubst (x typing.TermSubst) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.typing.TermSubst"), Body: core.TermMap_{Value: libmaps.Bimap(encodecore.Name).(func(any) any)(encodecore.Term).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func TypeConstraint (x typing.TypeConstraint) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.typing.TypeConstraint"), Fields: []any{core.Field{Name: core.Name("left"), Term: encodecore.Type_(func (v any) any {
    return v.(typing.TypeConstraint).Left
  }(x).(core.Type))}, core.Field{Name: core.Name("right"), Term: encodecore.Type_(func (v any) any {
    return v.(typing.TypeConstraint).Right
  }(x).(core.Type))}, core.Field{Name: core.Name("comment"), Term: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v.(typing.TypeConstraint).Comment
  }(x).(string)}}}}}}
}

func TypeSubst (x typing.TypeSubst) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.typing.TypeSubst"), Body: core.TermMap_{Value: libmaps.Bimap(encodecore.Name).(func(any) any)(encodecore.Type_).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}
