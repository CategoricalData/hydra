// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for working with qualified names.
 */



import * as Annotations from "./annotations.js";
import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Formatting from "./formatting.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMath from "./lib/math.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Relational from "./relational.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function compactName(namespaces: ReadonlyMap<Packaging.Namespace, string>): ((x: Core.Name) => string) {
  return ((name: Core.Name) => (() => {
  const qualName = qualifyName(name);
  const mns = ((_x) => _x.namespace)(qualName);
  const local = ((_x) => _x.local)(qualName);
  return LibMaybes.maybe(((_x) => _x)(name))(((ns: Packaging.Namespace) => LibMaybes.maybe(local)(((pre: string) => LibStrings.cat([pre, ":", local])))(LibMaps.lookup(ns)(namespaces))))(mns);
})());
}

export function freshName(cx: Context.Context): readonly [Core.Name, Context.Context] {
  return (() => {
  const count = Annotations.getCount(Constants.key_freshTypeVariableCount)(cx);
  return [normalTypeVariable(count), Annotations.putCount(Constants.key_freshTypeVariableCount)(LibMath.add(count)(1))(cx)];
})();
}

export function freshNames(n: number): ((x: Context.Context) => readonly [ReadonlyArray<Core.Name>, Context.Context]) {
  return ((cx: Context.Context) => (() => {
  const go = ((acc: readonly [ReadonlyArray<Core.Name>, Context.Context]) => ((_: t0) => (() => {
  const names = LibPairs.first(acc);
  return (() => {
  const cx0 = LibPairs.second(acc);
  return (() => {
  const result = freshName(cx0);
  return (() => {
  const name = LibPairs.first(result);
  return (() => {
  const cx1 = LibPairs.second(result);
  return [LibLists.concat2(names)(LibLists.pure(name)), cx1];
})();
})();
})();
})();
})()));
  return LibLists.foldl(go)([[], cx])(LibLists.replicate(n)(undefined));
})());
}

export function localNameOf(arg_: Core.Name): string {
  return ((_x) => _x.local)(qualifyName(arg_));
}

export function nameToFilePath(nsConv: Util.CaseConvention): ((x: Util.CaseConvention) => ((x: Packaging.FileExtension) => ((x: Core.Name) => string))) {
  return ((localConv: Util.CaseConvention) => ((ext: Packaging.FileExtension) => ((name: Core.Name) => (() => {
  const qualName = qualifyName(name);
  return (() => {
  const ns = ((_x) => _x.namespace)(qualName);
  return (() => {
  const local = ((_x) => _x.local)(qualName);
  return (() => {
  const nsToFilePath = ((ns2: Packaging.Namespace) => LibStrings.intercalate("/")(LibLists.map(((part: string) => Formatting.convertCase(({ tag: "camel" }))(nsConv)(part)))(LibStrings.splitOn(".")(((_x) => _x)(ns2)))));
  return (() => {
  const prefix = LibMaybes.maybe("")(((n: Packaging.Namespace) => LibStrings.cat2(nsToFilePath(n))("/")))(ns);
  return (() => {
  const suffix = Formatting.convertCase(({ tag: "pascal" }))(localConv)(local);
  return LibStrings.cat([prefix, suffix, ".", ((_x) => _x)(ext)]);
})();
})();
})();
})();
})();
})())));
}

export function namespaceOf(arg_: Core.Name): Packaging.Namespace | null {
  return ((_x) => _x.namespace)(qualifyName(arg_));
}

export function namespaceToFilePath(caseConv: Util.CaseConvention): ((x: Packaging.FileExtension) => ((x: Packaging.Namespace) => string)) {
  return ((ext: Packaging.FileExtension) => ((ns: Packaging.Namespace) => (() => {
  const parts = LibLists.map(((v1: string) => Formatting.convertCase(({ tag: "camel" }))(caseConv)(v1)))(LibStrings.splitOn(".")(((_x) => _x)(ns)));
  return LibStrings.cat2(LibStrings.cat2(LibStrings.intercalate("/")(parts))("."))(((_x) => _x)(ext));
})()));
}

export function normalTypeVariable(i: number): Core.Name {
  return LibStrings.cat2("t")(LibLiterals.showInt32(i));
}

export function qname(ns: Packaging.Namespace): ((x: string) => Core.Name) {
  return ((name: string) => LibStrings.cat([((_x) => _x)(ns), ".", name]));
}

export function qualifyName(name: Core.Name): Packaging.QualifiedName {
  return (() => {
  const parts = LibLists.reverse(LibStrings.splitOn(".")(((_x) => _x)(name)));
  return LibLogic.ifElse(LibEquality.equal(1)(LibLists.length(parts)))(({
    namespace: null,
    local: ((_x) => _x)(name)
  }))(({
    namespace: LibStrings.intercalate(".")(LibLists.reverse(LibLists.tail(parts))),
    local: LibLists.head(parts)
  }));
})();
}

export function uniqueLabel(visited: ReadonlySet<string>): ((x: string) => string) {
  return ((l: string) => LibLogic.ifElse(LibSets.member(l)(visited))(uniqueLabel(visited)(LibStrings.cat2(l)("'")))(l));
}

export function unqualifyName(qname: Packaging.QualifiedName): Core.Name {
  return (() => {
  const prefix = LibMaybes.maybe("")(((n: Packaging.Namespace) => LibStrings.cat2(((_x) => _x)(n))(".")))(((_x) => _x.namespace)(qname));
  return LibStrings.cat2(prefix)(((_x) => _x.local)(qname));
})();
}
