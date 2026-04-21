// Note: this is an automatically generated file. Do not edit.

/**
 * An interpretation of Codd's Relational Model, as described in 'A Relational Model of Data for Large Shared Data Banks' (1970). Types ('domains') and values are parameterized so as to allow for application-specific implementations. No special support is provided for 'nonsimple' domains; i.e. relations are assumed to be normalized.
 */



import * as Core from "./core.js";

export type ColumnName = string & { readonly __brand: "ColumnName" };

export interface ColumnSchema<t> {
  readonly name: ColumnName;
  readonly domain: t;
}

export interface ForeignKey {
  readonly foreignRelation: RelationName;
  readonly keys: ReadonlyMap<ColumnName, ColumnName>;
}

export type PrimaryKey = ReadonlyArray<ColumnName> & { readonly __brand: "PrimaryKey" };

export type Relation<v> = ReadonlyArray<Row<v>> & { readonly __brand: "Relation" };

export type RelationName = string & { readonly __brand: "RelationName" };

export interface RelationSchema<t> {
  readonly name: RelationName;
  readonly columns: ReadonlyArray<ColumnSchema<t>>;
  readonly primaryKeys: ReadonlyArray<PrimaryKey>;
  readonly foreignKeys: ReadonlyArray<ForeignKey>;
}

export type Relationship<v> = ReadonlySet<ReadonlyMap<ColumnName, v>> & { readonly __brand: "Relationship" };

export type Row<v> = ReadonlyArray<v> & { readonly __brand: "Row" };
