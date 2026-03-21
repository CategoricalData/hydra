package hydra.encode.relational

import hydra.core.*

import hydra.relational.*

import hydra.lib.lists

import hydra.lib.maps

import hydra.lib.sets

def columnName(x: hydra.relational.ColumnName): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.ColumnName", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def columnSchema[T0](t: (T0 => hydra.core.Term))(x: hydra.relational.ColumnSchema[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.relational.ColumnSchema", Seq(hydra.core.Field("name",
     hydra.encode.relational.columnName(x.name)), hydra.core.Field("domain", t(x.domain)))))

def foreignKey(x: hydra.relational.ForeignKey): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.relational.ForeignKey", Seq(hydra.core.Field("foreignRelation",
     hydra.encode.relational.relationName(x.foreignRelation)), hydra.core.Field("keys", hydra.core.Term.map(maps.bimap[hydra.relational.ColumnName,
     hydra.core.Term, hydra.relational.ColumnName, hydra.core.Term](hydra.encode.relational.columnName)(hydra.encode.relational.columnName)(x.keys))))))

def primaryKey(x: hydra.relational.PrimaryKey): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.PrimaryKey", hydra.core.Term.list(lists.map[hydra.relational.ColumnName,
     hydra.core.Term](hydra.encode.relational.columnName)(x))))

def relation[T0](v: (T0 => hydra.core.Term))(x: hydra.relational.Relation[T0]): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.Relation", hydra.core.Term.list(lists.map[hydra.relational.Row[T0],
     hydra.core.Term]((v1: hydra.relational.Row[T0]) => hydra.encode.relational.row(v)(v1))(x))))

def relationName(x: hydra.relational.RelationName): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.RelationName", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def relationSchema[T0](t: (T0 => hydra.core.Term))(x: hydra.relational.RelationSchema[T0]): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.relational.RelationSchema", Seq(hydra.core.Field("name",
     hydra.encode.relational.relationName(x.name)), hydra.core.Field("columns", hydra.core.Term.list(lists.map[hydra.relational.ColumnSchema[T0],
     hydra.core.Term]((v1: hydra.relational.ColumnSchema[T0]) => hydra.encode.relational.columnSchema(t)(v1))(x.columns))),
     hydra.core.Field("primaryKeys", hydra.core.Term.list(lists.map[hydra.relational.PrimaryKey, hydra.core.Term](hydra.encode.relational.primaryKey)(x.primaryKeys))),
     hydra.core.Field("foreignKeys", hydra.core.Term.list(lists.map[hydra.relational.ForeignKey, hydra.core.Term](hydra.encode.relational.foreignKey)(x.foreignKeys))))))

def relationship[T0](v: (T0 => hydra.core.Term))(x: hydra.relational.Relationship[T0]): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.Relationship", hydra.core.Term.set(sets.map[Map[hydra.relational.ColumnName,
     T0], hydra.core.Term]((m: Map[hydra.relational.ColumnName, T0]) =>
  hydra.core.Term.map(maps.bimap[hydra.relational.ColumnName, hydra.core.Term, T0, hydra.core.Term](hydra.encode.relational.columnName)(v)(m)))(x))))

def row[T0](v: (T0 => hydra.core.Term))(x: hydra.relational.Row[T0]): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.relational.Row", hydra.core.Term.list(lists.map[T0, hydra.core.Term](v)(x))))
