// Note: this is an automatically generated file. Do not edit.

package relational

type ColumnName string

type ColumnSchema [T any] struct {
  Name ColumnName
  Domain T
}

type ForeignKey struct {
  ForeignRelation RelationName
  Keys []any
}

type PrimaryKey []any

type Relation [V any] []any

type RelationName string

type RelationSchema [T any] struct {
  Name RelationName
  Columns []any
  PrimaryKeys []any
  ForeignKeys []any
}

type Relationship [V any] []any

type Row [V any] []any
