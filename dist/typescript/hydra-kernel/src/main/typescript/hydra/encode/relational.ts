// Note: this is an automatically generated file. Do not edit.

/**
 * Term encoders for hydra.relational
 */



import * as Core from "../core.js";
import * as EncodeCore from "./core.js";
import * as LibLists from "../lib/lists.js";
import * as LibMaps from "../lib/maps.js";
import * as LibSets from "../lib/sets.js";
import * as Relational from "../relational.js";

export function columnName(x: Relational.ColumnName): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.relational.ColumnName",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function columnSchema<t0>(t: ((x: t0) => Core.Term)): ((x: Relational.ColumnSchema<t0>) => Core.Term) {
  return ((x: Relational.ColumnSchema<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.relational.ColumnSchema",
    fields: [({
    name: "name",
    term: columnName(((_x) => _x.name)(x))
  }), ({
    name: "domain",
    term: t(((_x) => _x.domain)(x))
  })]
  }) }));
}

export function foreignKey(x: Relational.ForeignKey): Core.Term {
  return ({ tag: "record", value: ({
    typeName: "hydra.relational.ForeignKey",
    fields: [({
    name: "foreignRelation",
    term: relationName(((_x) => _x.foreignRelation)(x))
  }), ({
    name: "keys",
    term: ({ tag: "map", value: LibMaps.bimap(columnName)(columnName)(((_x) => _x.keys)(x)) })
  })]
  }) });
}

export function primaryKey(x: Relational.PrimaryKey): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.relational.PrimaryKey",
    body: ({ tag: "list", value: LibLists.map(columnName)(((_x) => _x)(x)) })
  }) });
}

export function relation<t0>(v: ((x: t0) => Core.Term)): ((x: Relational.Relation<t0>) => Core.Term) {
  return ((x: Relational.Relation<t0>) => ({ tag: "wrap", value: ({
    typeName: "hydra.relational.Relation",
    body: ({ tag: "list", value: LibLists.map(((v1: Relational.Row<t0>) => row(v)(v1)))(((_x) => _x)(x)) })
  }) }));
}

export function relationName(x: Relational.RelationName): Core.Term {
  return ({ tag: "wrap", value: ({
    typeName: "hydra.relational.RelationName",
    body: ({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(x) }) })
  }) });
}

export function relationSchema<t0>(t: ((x: t0) => Core.Term)): ((x: Relational.RelationSchema<t0>) => Core.Term) {
  return ((x: Relational.RelationSchema<t0>) => ({ tag: "record", value: ({
    typeName: "hydra.relational.RelationSchema",
    fields: [({
    name: "name",
    term: relationName(((_x) => _x.name)(x))
  }), ({
    name: "columns",
    term: ({ tag: "list", value: LibLists.map(((v1: Relational.ColumnSchema<t0>) => columnSchema(t)(v1)))(((_x) => _x.columns)(x)) })
  }), ({
    name: "primaryKeys",
    term: ({ tag: "list", value: LibLists.map(primaryKey)(((_x) => _x.primaryKeys)(x)) })
  }), ({
    name: "foreignKeys",
    term: ({ tag: "list", value: LibLists.map(foreignKey)(((_x) => _x.foreignKeys)(x)) })
  })]
  }) }));
}

export function relationship<t0>(v: ((x: t0) => Core.Term)): ((x: Relational.Relationship<t0>) => Core.Term) {
  return ((x: Relational.Relationship<t0>) => ({ tag: "wrap", value: ({
    typeName: "hydra.relational.Relationship",
    body: ({ tag: "set", value: LibSets.map(((m: ReadonlyMap<Relational.ColumnName, t0>) => ({ tag: "map", value: LibMaps.bimap(columnName)(v)(m) })))(((_x) => _x)(x)) })
  }) }));
}

export function row<t0>(v: ((x: t0) => Core.Term)): ((x: Relational.Row<t0>) => Core.Term) {
  return ((x: Relational.Row<t0>) => ({ tag: "wrap", value: ({
    typeName: "hydra.relational.Row",
    body: ({ tag: "list", value: LibLists.map(v)(((_x) => _x)(x)) })
  }) }));
}
