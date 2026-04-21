// Note: this is an automatically generated file. Do not edit.

/**
 * A simple, untyped tabular data model, suitable for CSVs and TSVs
 */



import * as Core from "./core.js";
import * as Relational from "./relational.js";

export interface ColumnType {
  readonly name: Relational.ColumnName;
  readonly type: Core.Type;
}

export type DataRow<v> = ReadonlyArray<v | null> & { readonly __brand: "DataRow" };

export type HeaderRow = ReadonlyArray<string> & { readonly __brand: "HeaderRow" };

export interface Table<v> {
  readonly header: HeaderRow | null;
  readonly data: ReadonlyArray<DataRow<v>>;
}

export interface TableType {
  readonly name: Relational.RelationName;
  readonly columns: ReadonlyArray<ColumnType>;
}
