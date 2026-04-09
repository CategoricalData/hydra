// Note: this is an automatically generated file. Do not edit.

package tabular

import (
  "hydra.dev/hydra/core"
  "hydra.dev/hydra/relational"
)

type ColumnType struct {
  Name relational.ColumnName
  Type_ core.Type
}

type DataRow [V any] []any

type HeaderRow []any

type Table [V any] struct {
  Header any
  Data []any
}

type TableType struct {
  Name relational.RelationName
  Columns []any
}
