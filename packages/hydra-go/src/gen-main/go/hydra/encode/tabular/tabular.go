// Note: this is an automatically generated file. Do not edit.

package encodetabular

import (
  "hydra.dev/hydra/core"
  encodecore "hydra.dev/hydra/encode/core"
  encoderelational "hydra.dev/hydra/encode/relational"
  liblists "hydra.dev/hydra/lib/lists"
  libmaybes "hydra.dev/hydra/lib/maybes"
  "hydra.dev/hydra/relational"
  "hydra.dev/hydra/tabular"
)

func ColumnType (x tabular.ColumnType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.tabular.ColumnType"), Fields: []any{core.Field{Name: core.Name("name"), Term: encoderelational.ColumnName(func (v any) any {
    return v.(tabular.ColumnType).Name
  }(x).(relational.ColumnName))}, core.Field{Name: core.Name("type"), Term: encodecore.Type_(func (v any) any {
    return v.(tabular.ColumnType).Type_
  }(x).(core.Type))}}}}
}

func DataRow[T0 any] (v func(T0) core.Term, x tabular.DataRow[T0]) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.tabular.DataRow"), Body: core.TermList{Value: liblists.Map(func (opt any) any {
    return core.TermMaybe{Value: libmaybes.Map(v).(func(any) any)(opt)}
  }).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func HeaderRow (x tabular.HeaderRow) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.tabular.HeaderRow"), Body: core.TermList{Value: liblists.Map(func (x2 string) any {
    return core.TermLiteral{Value: core.LiteralString_{Value: x2}}
  }).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func Table[T0 any] (v func(T0) core.Term, x tabular.Table[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.tabular.Table"), Fields: []any{core.Field{Name: core.Name("header"), Term: core.TermMaybe{Value: libmaybes.Map(HeaderRow).(func(any) any)(func (v any) any {
    return v.(tabular.Table[T0]).Header
  }(x))}}, core.Field{Name: core.Name("data"), Term: core.TermList{Value: liblists.Map(func (v1 tabular.DataRow[T0]) any {
    return DataRow[T0](v, v1)
  }).(func(any) any)(func (v any) any {
    return v.(tabular.Table[T0]).Data
  }(x)).([]any)}}}}}
}

func TableType (x tabular.TableType) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.tabular.TableType"), Fields: []any{core.Field{Name: core.Name("name"), Term: encoderelational.RelationName(func (v any) any {
    return v.(tabular.TableType).Name
  }(x).(relational.RelationName))}, core.Field{Name: core.Name("columns"), Term: core.TermList{Value: liblists.Map(ColumnType).(func(any) any)(func (v any) any {
    return v.(tabular.TableType).Columns
  }(x)).([]any)}}}}}
}
