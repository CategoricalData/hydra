// Note: this is an automatically generated file. Do not edit.

package encoderelational

import (
  "hydra.dev/hydra/core"
  liblists "hydra.dev/hydra/lib/lists"
  libmaps "hydra.dev/hydra/lib/maps"
  libsets "hydra.dev/hydra/lib/sets"
  "hydra.dev/hydra/relational"
)

func ColumnName (x relational.ColumnName) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.ColumnName"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func ColumnSchema[T0 any] (t func(T0) core.Term, x relational.ColumnSchema[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.relational.ColumnSchema"), Fields: []any{core.Field{Name: core.Name("name"), Term: ColumnName(func (v any) any {
    return v.(relational.ColumnSchema[T0]).Name
  }(x).(relational.ColumnName))}, core.Field{Name: core.Name("domain"), Term: t(func (v any) any {
    return v.(relational.ColumnSchema[T0]).Domain
  }(x).(T0))}}}}
}

func ForeignKey (x relational.ForeignKey) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.relational.ForeignKey"), Fields: []any{core.Field{Name: core.Name("foreignRelation"), Term: RelationName(func (v any) any {
    return v.(relational.ForeignKey).ForeignRelation
  }(x).(relational.RelationName))}, core.Field{Name: core.Name("keys"), Term: core.TermMap_{Value: libmaps.Bimap(ColumnName).(func(any) any)(ColumnName).(func(any) any)(func (v any) any {
    return v.(relational.ForeignKey).Keys
  }(x)).([]any)}}}}}
}

func PrimaryKey (x relational.PrimaryKey) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.PrimaryKey"), Body: core.TermList{Value: liblists.Map(ColumnName).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func Relation[T0 any] (v func(T0) core.Term, x relational.Relation[T0]) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.Relation"), Body: core.TermList{Value: liblists.Map(func (v1 relational.Row[T0]) any {
    return Row[T0](v, v1)
  }).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func RelationName (x relational.RelationName) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.RelationName"), Body: core.TermLiteral{Value: core.LiteralString_{Value: func (v any) any {
    return v
  }(x).(string)}}}}
}

func RelationSchema[T0 any] (t func(T0) core.Term, x relational.RelationSchema[T0]) core.Term {
  return core.TermRecord{Value: core.Record{TypeName: core.Name("hydra.relational.RelationSchema"), Fields: []any{core.Field{Name: core.Name("name"), Term: RelationName(func (v any) any {
    return v.(relational.RelationSchema[T0]).Name
  }(x).(relational.RelationName))}, core.Field{Name: core.Name("columns"), Term: core.TermList{Value: liblists.Map(func (v1 relational.ColumnSchema[T0]) any {
    return ColumnSchema[T0](t, v1)
  }).(func(any) any)(func (v any) any {
    return v.(relational.RelationSchema[T0]).Columns
  }(x)).([]any)}}, core.Field{Name: core.Name("primaryKeys"), Term: core.TermList{Value: liblists.Map(PrimaryKey).(func(any) any)(func (v any) any {
    return v.(relational.RelationSchema[T0]).PrimaryKeys
  }(x)).([]any)}}, core.Field{Name: core.Name("foreignKeys"), Term: core.TermList{Value: liblists.Map(ForeignKey).(func(any) any)(func (v any) any {
    return v.(relational.RelationSchema[T0]).ForeignKeys
  }(x)).([]any)}}}}}
}

func Relationship[T0 any] (v func(T0) core.Term, x relational.Relationship[T0]) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.Relationship"), Body: core.TermSet{Value: libsets.Map(func (m []any) any {
    return core.TermMap_{Value: libmaps.Bimap(ColumnName).(func(any) any)(v).(func(any) any)(m).([]any)}
  }).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}

func Row[T0 any] (v func(T0) core.Term, x relational.Row[T0]) core.Term {
  return core.TermWrap{Value: core.WrappedTerm{TypeName: core.Name("hydra.relational.Row"), Body: core.TermList{Value: liblists.Map(v).(func(any) any)(func (v any) any {
    return v
  }(x)).([]any)}}}
}
