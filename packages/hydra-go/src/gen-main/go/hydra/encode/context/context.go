// Note: this is an automatically generated file. Do not edit.

package encodecontext

import (
  "hydra.dev/hydra/context"
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
)

func Context (x context.Context) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.context.Context"), Fields: []any{core.Field{Name: core.Name("trace"), Term: core.TermList{Value: liblists.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(context.Context).Trace
  }(x)).([]any)}}, core.Field{Name: core.Name("messages"), Term: core.TermList{Value: liblists.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v.(context.Context).Messages
  }(x)).([]any)}}, core.Field{Name: core.Name("other"), Term: core.TermMap_{Value: libmaps.Bimap(encodecore.Name).(func(any) any)(encodecore.Term).(func(any) any)(func (v any) any {
    return v.(context.Context).Other
  }(x)).([]any)}}}}}
}

func InContext[T0 any] (e func(T0) core.Term, x context.InContext[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.context.InContext"), Fields: []any{core.Field{Name: core.Name("object"), Term: e(func (v any) any {
    return v.(context.InContext[T0]).Object
  }(x).(T0))}, core.Field{Name: core.Name("context"), Term: Context(func (v any) any {
    return v.(context.InContext[T0]).Context
  }(x).(context.Context))}}}}
}
