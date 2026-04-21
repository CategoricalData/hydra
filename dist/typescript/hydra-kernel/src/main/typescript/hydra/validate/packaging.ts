// Note: this is an automatically generated file. Do not edit.

/**
 * Validation functions for modules and packages
 */



import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function checkConflictingModuleNamespaces(pkg: Packaging.Package): ErrorPackaging.InvalidPackageError | null {
  return (() => {
  const result = LibLists.foldl(((acc: readonly [ReadonlyMap<string, Packaging.Namespace>, ErrorPackaging.InvalidPackageError | null]) => ((mod: Packaging.Module) => (() => {
  const seen = LibPairs.first(acc);
  return (() => {
  const err = LibPairs.second(acc);
  return LibMaybes.cases(err)((() => {
  const ns = ((_x) => _x.namespace)(mod);
  return (() => {
  const key = LibStrings.toLower(((_x) => _x)(ns));
  return (() => {
  const existing = LibMaps.lookup(key)(seen);
  return LibMaybes.cases(existing)([LibMaps.insert(key)(ns)(seen), null])(((first: Packaging.Namespace) => [seen, ({ tag: "conflictingModuleNamespace", value: ({
    first: first,
    second: ns
  }) })]));
})();
})();
})())(((_: ErrorPackaging.InvalidPackageError) => acc));
})();
})())))([LibMaps.empty, null])(((_x) => _x.modules)(pkg));
  return LibPairs.second(result);
})();
}

export function checkConflictingVariantNames(mod: Packaging.Module): ErrorPackaging.InvalidModuleError | null {
  return (() => {
  const ns = ((_x) => _x.namespace)(mod);
  return (() => {
  const defs = ((_x) => _x.definitions)(mod);
  return (() => {
  const defNames = LibLists.foldl(((acc: ReadonlySet<string>) => ((def: Packaging.Definition) => LibSets.insert(Names.localNameOf(definitionName(def)))(acc))))(LibSets.empty)(defs);
  return LibLists.foldl(((acc: ErrorPackaging.InvalidModuleError | null) => ((def: Packaging.Definition) => LibMaybes.cases(acc)((() => {
  const _m = def;
  switch (_m.tag) {
    case "type": return ((td: Packaging.TypeDefinition) => (() => {
  const typeName = ((_x) => _x.name)(td);
  return (() => {
  const localTypeName = Names.localNameOf(typeName);
  return (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(td));
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "union": return ((fields: ReadonlyArray<Core.FieldType>) => LibLists.foldl(((innerAcc: ErrorPackaging.InvalidModuleError | null) => ((field: Core.FieldType) => LibMaybes.cases(innerAcc)((() => {
  const fieldName = ((_x) => _x.name)(field);
  return (() => {
  const localFieldName = Names.localNameOf(fieldName);
  return (() => {
  const constructorName = LibStrings.cat2(Formatting.capitalize(localTypeName))(Formatting.capitalize(localFieldName));
  return LibLogic.ifElse(LibSets.member(constructorName)(defNames))(({ tag: "conflictingVariantName", value: ({
    namespace: ns,
    typeName: typeName,
    variantName: fieldName,
    conflictingName: constructorName
  }) }))(null);
})();
})();
})())(((_: ErrorPackaging.InvalidModuleError) => innerAcc)))))(null)(fields))((_m as any).value);
    default: return null(_m);
  }
})();
})();
})();
})())((_m as any).value);
    default: return null(_m);
  }
})())(((_: ErrorPackaging.InvalidModuleError) => acc)))))(null)(defs);
})();
})();
})();
}

export function checkDefinitionNamespaces(mod: Packaging.Module): ErrorPackaging.InvalidModuleError | null {
  return (() => {
  const ns = ((_x) => _x.namespace)(mod);
  return (() => {
  const prefix = LibStrings.cat2(((_x) => _x)(ns))(".");
  return (() => {
  const prefixLen = LibStrings.length(prefix);
  return LibLists.foldl(((acc: ErrorPackaging.InvalidModuleError | null) => ((def: Packaging.Definition) => LibMaybes.cases(acc)((() => {
  const name = definitionName(def);
  return (() => {
  const nameStr = ((_x) => _x)(name);
  return (() => {
  const namePrefix = LibLists.take(prefixLen)(LibStrings.toList(nameStr));
  return LibLogic.ifElse(LibEquality.equal(LibStrings.fromList(namePrefix))(prefix))(null)(({ tag: "definitionNotInModuleNamespace", value: ({
    namespace: ns,
    name: name
  }) }));
})();
})();
})())(((_: ErrorPackaging.InvalidModuleError) => acc)))))(null)(((_x) => _x.definitions)(mod));
})();
})();
})();
}

export function checkDuplicateDefinitionNames(mod: Packaging.Module): ErrorPackaging.InvalidModuleError | null {
  return (() => {
  const ns = ((_x) => _x.namespace)(mod);
  return (() => {
  const result = LibLists.foldl(((acc: readonly [ReadonlySet<Core.Name>, ErrorPackaging.InvalidModuleError | null]) => ((def: Packaging.Definition) => (() => {
  const seen = LibPairs.first(acc);
  return (() => {
  const err = LibPairs.second(acc);
  return LibMaybes.cases(err)((() => {
  const name = definitionName(def);
  return LibLogic.ifElse(LibSets.member(name)(seen))([seen, ({ tag: "duplicateDefinitionName", value: ({
    namespace: ns,
    name: name
  }) })])([LibSets.insert(name)(seen), null]);
})())(((_: ErrorPackaging.InvalidModuleError) => acc));
})();
})())))([LibSets.empty, null])(((_x) => _x.definitions)(mod));
  return LibPairs.second(result);
})();
})();
}

export function checkDuplicateModuleNamespaces(pkg: Packaging.Package): ErrorPackaging.InvalidPackageError | null {
  return (() => {
  const result = LibLists.foldl(((acc: readonly [ReadonlySet<Packaging.Namespace>, ErrorPackaging.InvalidPackageError | null]) => ((mod: Packaging.Module) => (() => {
  const seen = LibPairs.first(acc);
  return (() => {
  const err = LibPairs.second(acc);
  return LibMaybes.cases(err)((() => {
  const ns = ((_x) => _x.namespace)(mod);
  return LibLogic.ifElse(LibSets.member(ns)(seen))([seen, ({ tag: "duplicateModuleNamespace", value: ({
    namespace: ns
  }) })])([LibSets.insert(ns)(seen), null]);
})())(((_: ErrorPackaging.InvalidPackageError) => acc));
})();
})())))([LibSets.empty, null])(((_x) => _x.modules)(pkg));
  return LibPairs.second(result);
})();
}

export function definitionName(def: Packaging.Definition): Core.Name {
  return (() => {
  const _m = def;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ((_x) => _x.name)(td))((_m as any).value);
    case "type": return ((td: Packaging.TypeDefinition) => ((_x) => _x.name)(td))((_m as any).value);
  }
})();
}

export function module(mod: Packaging.Module): ErrorPackaging.InvalidModuleError | null {
  return (() => {
  const r1 = checkDefinitionNamespaces(mod);
  return LibMaybes.cases(r1)((() => {
  const r2 = checkDuplicateDefinitionNames(mod);
  return LibMaybes.cases(r2)(checkConflictingVariantNames(mod))(((_: ErrorPackaging.InvalidModuleError) => r2));
})())(((_: ErrorPackaging.InvalidModuleError) => r1));
})();
}

export function package_(pkg: Packaging.Package): ErrorPackaging.InvalidPackageError | null {
  return (() => {
  const r1 = checkDuplicateModuleNamespaces(pkg);
  return LibMaybes.cases(r1)((() => {
  const r2 = checkConflictingModuleNamespaces(pkg);
  return LibMaybes.cases(r2)(LibLists.foldl(((acc: ErrorPackaging.InvalidPackageError | null) => ((mod: Packaging.Module) => LibMaybes.cases(acc)(LibMaybes.map(((err: ErrorPackaging.InvalidModuleError) => ({ tag: "invalidModule", value: err })))(module(mod)))(((_: ErrorPackaging.InvalidPackageError) => acc)))))(null)(((_x) => _x.modules)(pkg)))(((_: ErrorPackaging.InvalidPackageError) => r2));
})())(((_: ErrorPackaging.InvalidPackageError) => r1));
})();
}
