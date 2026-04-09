// Note: this is an automatically generated file. Do not edit.

package encodetopology

import (
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/topology"
)

func Graph (m []any) core.Term {
  return core.TermMap_{Value: libmaps.Bimap(Vertex).(func(any) any)(func (xs []any) any {
    return core.TermList{Value: liblists.Map(Vertex).(func(any) any)(xs).([]any)}
  }).(func(any) any)(m).([]any)}
}

func TarjanState (x topology.TarjanState) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.topology.TarjanState"), Fields: []any{core.Field{Name: core.Name("counter"), Term: core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: func (v any) any {
    return v.(topology.TarjanState).Counter
  }(x).(int32)}}}}, core.Field{Name: core.Name("indices"), Term: core.TermMap_{Value: libmaps.Bimap(Vertex).(func(any) any)(func (x2 int32) any {
    return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
  }).(func(any) any)(func (v any) any {
    return v.(topology.TarjanState).Indices
  }(x)).([]any)}}, core.Field{Name: core.Name("lowLinks"), Term: core.TermMap_{Value: libmaps.Bimap(Vertex).(func(any) any)(func (x2 int32) any {
    return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x2}}}
  }).(func(any) any)(func (v any) any {
    return v.(topology.TarjanState).LowLinks
  }(x)).([]any)}}, core.Field{Name: core.Name("stack"), Term: core.TermList{Value: liblists.Map(Vertex).(func(any) any)(func (v any) any {
    return v.(topology.TarjanState).Stack
  }(x)).([]any)}}, core.Field{Name: core.Name("onStack"), Term: core.TermSet{Value: libsets.Map(Vertex).(func(any) any)(func (v any) any {
    return v.(topology.TarjanState).OnStack
  }(x)).([]any)}}, core.Field{Name: core.Name("sccs"), Term: core.TermList{Value: liblists.Map(func (xs2 []any) any {
    return core.TermList{Value: liblists.Map(Vertex).(func(any) any)(xs2).([]any)}
  }).(func(any) any)(func (v any) any {
    return v.(topology.TarjanState).Sccs
  }(x)).([]any)}}}}}
}

func Vertex (x int32) core.Term {
  return core.TermLiteral{Value: core.LiteralInteger{Value: core.IntegerValueInt32_{Value: x}}}
}
