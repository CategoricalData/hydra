// Note: this is an automatically generated file. Do not edit.

/**
 * Utilities for working with Haskell syntax trees
 */



import * as Analysis from "../analysis.js";
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
import * as HaskellLanguage from "./language.js";
import * as HaskellSyntax from "./syntax.js";
import * as JsonModel from "../json/model.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMath from "../lib/math.js";
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
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variants from "../variants.js";

export function applicationPattern(name: HaskellSyntax.Name): ((x: ReadonlyArray<HaskellSyntax.Pattern>) => HaskellSyntax.Pattern) {
  return ((args: ReadonlyArray<HaskellSyntax.Pattern>) => ({ tag: "application", value: ({
    name: name,
    args: args
  }) }));
}

export function elementReference(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => HaskellSyntax.Name) {
  return ((name: Core.Name) => (() => {
  const namespacePair = ((_x) => _x.focus)(namespaces);
  const gname = LibPairs.first(namespacePair);
  const gmod = ((_x) => _x)(LibPairs.second(namespacePair));
  const namespacesMap = ((_x) => _x.mapping)(namespaces);
  const qname = Names.qualifyName(name);
  const local = ((_x) => _x.local)(qname);
  const escLocal = sanitizeHaskellName(local);
  const mns = ((_x) => _x.namespace)(qname);
  return LibMaybes.cases(((_x) => _x.namespace)(qname))(simpleName(local))(((ns: Packaging.Namespace) => LibMaybes.cases(LibMaps.lookup(ns)(namespacesMap))(simpleName(local))(((mn: HaskellSyntax.ModuleName) => (() => {
  const aliasStr = ((_x) => _x)(mn);
  return LibLogic.ifElse(LibEquality.equal(ns)(gname))(simpleName(escLocal))(rawName(LibStrings.cat([aliasStr, ".", sanitizeHaskellName(local)])));
})()))));
})());
}

export function hsapp(l: HaskellSyntax.Expression): ((x: HaskellSyntax.Expression) => HaskellSyntax.Expression) {
  return ((r: HaskellSyntax.Expression) => ({ tag: "application", value: ({
    function: l,
    argument: r
  }) }));
}

export function hslambda(name: HaskellSyntax.Name): ((x: HaskellSyntax.Expression) => HaskellSyntax.Expression) {
  return ((rhs: HaskellSyntax.Expression) => ({ tag: "lambda", value: ({
    bindings: [({ tag: "name", value: name })],
    inner: rhs
  }) }));
}

export function hslit(lit: HaskellSyntax.Literal): HaskellSyntax.Expression {
  return ({ tag: "literal", value: lit });
}

export function hsvar(s: string): HaskellSyntax.Expression {
  return ({ tag: "variable", value: rawName(s) });
}

export function namespacesForModule<t0>(mod: Packaging.Module): ((x: t0) => ((x: Graph.Graph) => Errors.Error | Packaging.Namespaces<HaskellSyntax.ModuleName>)) {
  return ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(Analysis.moduleDependencyNamespaces(cx)(g)(true)(true)(true)(true)(mod))(((nss: ReadonlySet<Packaging.Namespace>) => (() => {
  const ns = ((_x) => _x.namespace)(mod);
  return (() => {
  const segmentsOf = ((namespace: Packaging.Namespace) => LibStrings.splitOn(".")(((_x) => _x)(namespace)));
  return (() => {
  const aliasFromSuffix = ((segs: ReadonlyArray<string>) => ((n: number) => (() => {
  const dropCount = LibMath.sub(LibLists.length(segs))(n);
  const suffix = LibLists.drop(dropCount)(segs);
  const capitalizedSuffix = LibLists.map(Formatting.capitalize)(suffix);
  return LibStrings.cat(capitalizedSuffix);
})()));
  return (() => {
  const toModuleName = ((namespace: Packaging.Namespace) => aliasFromSuffix(segmentsOf(namespace))(1));
  return (() => {
  const focusPair = [ns, toModuleName(ns)];
  return (() => {
  const nssAsList = LibSets.toList(nss);
  return (() => {
  const segsMap = LibMaps.fromList(LibLists.map(((nm: Packaging.Namespace) => [nm, segmentsOf(nm)]))(nssAsList));
  return (() => {
  const maxSegs = LibLists.foldl(((a: number) => ((b: number) => LibLogic.ifElse(LibEquality.gt(a)(b))(a)(b))))(1)(LibLists.map(((nm: Packaging.Namespace) => LibLists.length(segmentsOf(nm))))(nssAsList));
  return (() => {
  const initialState = LibMaps.fromList(LibLists.map(((nm: Packaging.Namespace) => [nm, 1]))(nssAsList));
  return (() => {
  const segsFor = ((nm: Packaging.Namespace) => LibMaybes.fromMaybe([])(LibMaps.lookup(nm)(segsMap)));
  return (() => {
  const takenFor = ((state: ReadonlyMap<t1, number>) => ((nm: t1) => LibMaybes.fromMaybe(1)(LibMaps.lookup(nm)(state))));
  return (() => {
  const growStep = ((state: ReadonlyMap<Packaging.Namespace, number>) => ((_ign: t1) => (() => {
  const aliasEntries = LibLists.map(((nm: Packaging.Namespace) => (() => {
  const segs = segsFor(nm);
  const n = takenFor(state)(nm);
  const segCount = LibLists.length(segs);
  const aliasStr = ((_x) => _x)(aliasFromSuffix(segs)(n));
  return [nm, [n, [segCount, aliasStr]]];
})()))(nssAsList);
  const aliasCounts = LibLists.foldl(((m: ReadonlyMap<string, number>) => ((e: readonly [Packaging.Namespace, readonly [number, readonly [number, string]]]) => (() => {
  const k = LibPairs.second(LibPairs.second(LibPairs.second(e)));
  return LibMaps.insert(k)(LibMath.add(1)(LibMaybes.fromMaybe(0)(LibMaps.lookup(k)(m))))(m);
})())))(LibMaps.empty)(aliasEntries);
  const aliasMinSegs = LibLists.foldl(((m: ReadonlyMap<string, number>) => ((e: readonly [Packaging.Namespace, readonly [number, readonly [number, string]]]) => (() => {
  const segCount = LibPairs.first(LibPairs.second(LibPairs.second(e)));
  const k = LibPairs.second(LibPairs.second(LibPairs.second(e)));
  const existing = LibMaps.lookup(k)(m);
  return LibMaps.insert(k)(LibMaybes.cases(existing)(segCount)(((prev: number) => LibLogic.ifElse(LibEquality.lt(segCount)(prev))(segCount)(prev))))(m);
})())))(LibMaps.empty)(aliasEntries);
  return (() => {
  const aliasMinSegsCount = LibLists.foldl(((m: ReadonlyMap<string, number>) => ((e: readonly [Packaging.Namespace, readonly [number, readonly [number, string]]]) => (() => {
  const segCount = LibPairs.first(LibPairs.second(LibPairs.second(e)));
  const k = LibPairs.second(LibPairs.second(LibPairs.second(e)));
  const minSegs = LibMaybes.fromMaybe(segCount)(LibMaps.lookup(k)(aliasMinSegs));
  return LibLogic.ifElse(LibEquality.equal(segCount)(minSegs))(LibMaps.insert(k)(LibMath.add(1)(LibMaybes.fromMaybe(0)(LibMaps.lookup(k)(m))))(m))(m);
})())))(LibMaps.empty)(aliasEntries);
  return LibMaps.fromList(LibLists.map(((e: readonly [Packaging.Namespace, readonly [number, readonly [number, string]]]) => (() => {
  const nm = LibPairs.first(e);
  const n = LibPairs.first(LibPairs.second(e));
  const segCount = LibPairs.first(LibPairs.second(LibPairs.second(e)));
  const aliasStr = LibPairs.second(LibPairs.second(LibPairs.second(e)));
  const count = LibMaybes.fromMaybe(0)(LibMaps.lookup(aliasStr)(aliasCounts));
  const minSegs = LibMaybes.fromMaybe(segCount)(LibMaps.lookup(aliasStr)(aliasMinSegs));
  const minSegsCount = LibMaybes.fromMaybe(0)(LibMaps.lookup(aliasStr)(aliasMinSegsCount));
  const canGrow = LibLogic.and(LibEquality.gt(count)(1))(LibLogic.and(LibEquality.gt(segCount)(n))(LibLogic.or(LibEquality.gt(segCount)(minSegs))(LibEquality.gt(minSegsCount)(1))));
  const newN = LibLogic.ifElse(canGrow)(LibMath.add(n)(1))(n);
  return [nm, newN];
})()))(aliasEntries));
})();
})()));
  return (() => {
  const finalState = LibLists.foldl(growStep)(initialState)(LibLists.replicate(maxSegs)(undefined));
  return (() => {
  const resultMap = LibMaps.fromList(LibLists.map(((nm: Packaging.Namespace) => [nm, aliasFromSuffix(segsFor(nm))(takenFor(finalState)(nm))]))(nssAsList));
  return ({ tag: "right", value: ({
    focus: focusPair,
    mapping: resultMap
  }) });
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()))));
}

export function newtypeAccessorName(name: Core.Name): string {
  return LibStrings.cat2("un")(Names.localNameOf(name));
}

export function rawName(n: string): HaskellSyntax.Name {
  return ({ tag: "normal", value: ({
    qualifiers: [],
    unqualified: n
  }) });
}

export function recordFieldReference(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => ((x: Core.Name) => HaskellSyntax.Name)) {
  return ((sname: Core.Name) => ((fname: Core.Name) => (() => {
  const fnameStr = ((_x) => _x)(fname);
  const qname = Names.qualifyName(sname);
  const ns = ((_x) => _x.namespace)(qname);
  const typeNameStr = typeNameForRecord(sname);
  const decapitalized = Formatting.decapitalize(typeNameStr);
  const capitalized = Formatting.capitalize(fnameStr);
  const nm = LibStrings.cat2(decapitalized)(capitalized);
  const qualName = ({
    namespace: ns,
    local: nm
  });
  const unqualName = Names.unqualifyName(qualName);
  return elementReference(namespaces)(unqualName);
})()));
}

export function sanitizeHaskellName(v1: string): string {
  return Formatting.sanitizeWithUnderscores(HaskellLanguage.reservedWords)(v1);
}

export function simpleName(arg_: string): HaskellSyntax.Name {
  return rawName(sanitizeHaskellName(arg_));
}

export function simpleValueBinding(hname: HaskellSyntax.Name): ((x: HaskellSyntax.Expression) => ((x: HaskellSyntax.LocalBindings | null) => HaskellSyntax.ValueBinding)) {
  return ((rhs: HaskellSyntax.Expression) => ((bindings: HaskellSyntax.LocalBindings | null) => (() => {
  const pat = ({ tag: "application", value: ({
    name: hname,
    args: []
  }) });
  const rightHandSide = rhs;
  return ({ tag: "simple", value: ({
    pattern: pat,
    rhs: rightHandSide,
    localBindings: bindings
  }) });
})()));
}

export function toTypeApplication(types: ReadonlyArray<HaskellSyntax.Type>): HaskellSyntax.Type {
  return (() => {
  const app = ((l: ReadonlyArray<HaskellSyntax.Type>) => LibLogic.ifElse(LibEquality.gt(LibLists.length(l))(1))(({ tag: "application", value: ({
    context: app(LibLists.tail(l)),
    argument: LibLists.head(l)
  }) }))(LibLists.head(l)));
  return app(LibLists.reverse(types));
})();
}

export function typeNameForRecord(sname: Core.Name): string {
  return (() => {
  const snameStr = ((_x) => _x)(sname);
  const parts = LibStrings.splitOn(".")(snameStr);
  return LibLists.last(parts);
})();
}

export function unionFieldReference(boundNames: ReadonlySet<Core.Name>): ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((x: Core.Name) => ((x: Core.Name) => HaskellSyntax.Name))) {
  return ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((sname: Core.Name) => ((fname: Core.Name) => (() => {
  const fnameStr = ((_x) => _x)(fname);
  const qname = Names.qualifyName(sname);
  const ns = ((_x) => _x.namespace)(qname);
  const typeNameStr = typeNameForRecord(sname);
  const capitalizedTypeName = Formatting.capitalize(typeNameStr);
  const capitalizedFieldName = Formatting.capitalize(fnameStr);
  const deconflict = ((name: string) => (() => {
  const tname = Names.unqualifyName(({
    namespace: ns,
    local: name
  }));
  return LibLogic.ifElse(LibSets.member(tname)(boundNames))(deconflict(LibStrings.cat2(name)("_")))(name);
})());
  const nm = deconflict(LibStrings.cat2(capitalizedTypeName)(capitalizedFieldName));
  const qualName = ({
    namespace: ns,
    local: nm
  });
  const unqualName = Names.unqualifyName(qualName);
  return elementReference(namespaces)(unqualName);
})())));
}

export function unpackForallType(t: Core.Type): readonly [ReadonlyArray<Core.Name>, Core.Type] {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((fat: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(fat);
  const tbody = ((_x) => _x.body)(fat);
  const recursiveResult = unpackForallType(tbody);
  const vars = LibPairs.first(recursiveResult);
  const finalType = LibPairs.second(recursiveResult);
  return [LibLists.cons(v)(vars), finalType];
})())((_m as any).value);
    default: return [[], t](_m);
  }
})();
}
