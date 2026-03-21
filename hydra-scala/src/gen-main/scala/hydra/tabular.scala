package hydra.tabular

import hydra.core.*

import hydra.relational.*

import hydra.core

import hydra.relational

case class ColumnType(name: hydra.relational.ColumnName, `type`: hydra.core.Type)

type DataRow [V] = Seq[Option[V]]

type HeaderRow = Seq[scala.Predef.String]

case class Table[V](header: Option[hydra.tabular.HeaderRow], data: Seq[hydra.tabular.DataRow[V]])

case class TableType(name: hydra.relational.RelationName, columns: Seq[hydra.tabular.ColumnType])
