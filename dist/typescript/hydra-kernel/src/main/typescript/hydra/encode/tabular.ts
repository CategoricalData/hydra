// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.tabular
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as EncodeRelational from "./relational.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaybes from "../lib/maybes.js";
import * as Tabular from "../tabular.js";

export function columnType(x: Tabular.ColumnType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.tabular.ColumnType",
    fields: [({
    name: "name",
    term: EncodeRelational.columnName(((_x) => _x.name)(x))
  }), ({
    name: "type",
    term: EncodeCore.type(((_x) => _x.type)(x))
  })]
  }) });
}

export function dataRow<t0>(v: ((x: t0) => Core.Term)): ((x: Tabular.DataRow<t0>) => Core.Term) {
  return ((x: Tabular.DataRow<t0>) => ({ tag: "wrap", value: ({
    typeName: "hydra.tabular.DataRow",
    body: ({ tag: "list", value: LibLists.map(((opt: t0 | null) => ({ tag: "maybe", value: LibMaybes.map(v)(opt) })))(((_x) => _x)(x)) })
  }) }));
}

export function headerRow(x: Tabular.HeaderRow): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.tabular.HeaderRow",
    body: ({ tag: "list", value: LibLists.map(((x2: string) => ({ tag: "literal", value: ({ tag: "string", value: x2 }) })))(((_x) => _x)(x)) })
  }) });
}

export function table<t0>(v: ((x: t0) => Core.Term)): ((x: Tabular.Table<t0>) => Core.Term) {
  return ((x: Tabular.Table<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.tabular.Table",
    fields: [({
    name: "header",
    term: ({ tag: "maybe", value: LibMaybes.map(headerRow)(((_x) => _x.header)(x)) })
  }), ({
    name: "data",
    term: ({ tag: "list", value: LibLists.map(((v1: Tabular.DataRow<t0>) => dataRow(v)(v1)))(((_x) => _x.data)(x)) })
  })]
  }) }));
}

export function tableType(x: Tabular.TableType): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.tabular.TableType",
    fields: [({
    name: "name",
    term: EncodeRelational.relationName(((_x) => _x.name)(x))
  }), ({
    name: "columns",
    term: ({ tag: "list", value: LibLists.map(columnType)(((_x) => _x.columns)(x)) })
  })]
  }) });
}
