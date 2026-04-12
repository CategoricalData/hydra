package hydra.encode.tabular

import hydra.core.*

import hydra.tabular.*

def columnType(x: hydra.tabular.ColumnType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.tabular.ColumnType", Seq(hydra.core.Field("name", hydra.encode.relational.columnName(x.name)),
     hydra.core.Field("type", hydra.encode.core.`type`(x.`type`)))))

def dataRow[T0](v: (T0 => hydra.core.Term))(x: hydra.tabular.DataRow[T0]): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.tabular.DataRow", hydra.core.Term.list(hydra.lib.lists.map[Option[T0],
     hydra.core.Term]((opt: Option[T0]) =>
  hydra.core.Term.maybe(hydra.lib.maybes.map[T0, hydra.core.Term](v)(opt)))(x))))

def headerRow(x: hydra.tabular.HeaderRow): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.tabular.HeaderRow", hydra.core.Term.list(hydra.lib.lists.map[scala.Predef.String,
     hydra.core.Term]((x2: scala.Predef.String) => hydra.core.Term.literal(hydra.core.Literal.string(x2)))(x))))

def table[T0](v: (T0 => hydra.core.Term))(x: hydra.tabular.Table[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.tabular.Table", Seq(hydra.core.Field("header", hydra.core.Term.maybe(hydra.lib.maybes.map[hydra.tabular.HeaderRow,
     hydra.core.Term](hydra.encode.tabular.headerRow)(x.header))), hydra.core.Field("data", hydra.core.Term.list(hydra.lib.lists.map[hydra.tabular.DataRow[T0],
     hydra.core.Term]((v1: hydra.tabular.DataRow[T0]) => hydra.encode.tabular.dataRow(v)(v1))(x.data))))))

def tableType(x: hydra.tabular.TableType): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.tabular.TableType", Seq(hydra.core.Field("name", hydra.encode.relational.relationName(x.name)),
     hydra.core.Field("columns", hydra.core.Term.list(hydra.lib.lists.map[hydra.tabular.ColumnType, hydra.core.Term](hydra.encode.tabular.columnType)(x.columns))))))
