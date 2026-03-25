package hydra.relational

import hydra.core.*

type ColumnName = scala.Predef.String

case class ColumnSchema[T](name: hydra.relational.ColumnName, domain: T)

case class ForeignKey(foreignRelation: hydra.relational.RelationName, keys: Map[hydra.relational.ColumnName, hydra.relational.ColumnName])

type PrimaryKey = Seq[hydra.relational.ColumnName]

type Relation [V] = Seq[hydra.relational.Row[V]]

type RelationName = scala.Predef.String

case class RelationSchema[T](name: hydra.relational.RelationName, columns: Seq[hydra.relational.ColumnSchema[T]], primaryKeys: Seq[hydra.relational.PrimaryKey], foreignKeys: Seq[hydra.relational.ForeignKey])

type Relationship [V] = scala.collection.immutable.Set[Map[hydra.relational.ColumnName, V]]

type Row [V] = Seq[V]
