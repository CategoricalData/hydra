// Note: this is an automatically generated file. Do not edit.

/**
 * Type and term classification predicates
 */



import * as Arity from "./arity.js";
import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as DecodeCore from "./decode/core.js";
import * as Dependencies from "./dependencies.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Reflect from "./reflect.js";
import * as Relational from "./relational.js";
import * as Rewriting from "./rewriting.js";
import * as Strip from "./strip.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variants from "./variants.js";

export function isComplexBinding(tc: Graph.Graph): ((x: Core.Binding) => boolean) {
  return ((b: Core.Binding) => (() => {
  const term = ((_x) => _x.term)(b);
  return (() => {
  const mts = ((_x) => _x.type)(b);
  return LibMaybes.cases(mts)(isComplexTerm(tc)(term))(((ts: Core.TypeScheme) => (() => {
  const isPolymorphic = LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts)));
  return (() => {
  const isNonNullary = LibEquality.gt(Arity.typeArity(((_x) => _x.type)(ts)))(0);
  return (() => {
  const isComplex = isComplexTerm(tc)(term);
  return LibLogic.or(LibLogic.or(isPolymorphic)(isNonNullary))(isComplex);
})();
})();
})()));
})();
})());
}

export function isComplexTerm(tc: Graph.Graph): ((x: Core.Term) => boolean) {
  return ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "let": return ((_: Core.Let) => true)((_m as any).value);
    case "typeApplication": return ((_: Core.TypeApplicationTerm) => true)((_m as any).value);
    case "typeLambda": return ((_: Core.TypeLambda) => true)((_m as any).value);
    case "variable": return ((name: Core.Name) => isComplexVariable(tc)(name))((_m as any).value);
    default: return LibLists.foldl(((b: boolean) => ((sub: Core.Term) => LibLogic.or(b)(isComplexTerm(tc)(sub)))))(false)(Rewriting.subterms(t))(_m);
  }
})());
}

export function isComplexVariable(tc: Graph.Graph): ((x: Core.Name) => boolean) {
  return ((name: Core.Name) => (() => {
  const metaLookup = LibMaps.lookup(name)(((_x) => _x.metadata)(tc));
  return LibLogic.ifElse(LibMaybes.isJust(metaLookup))(true)(LibLogic.ifElse(LibSets.member(name)(((_x) => _x.lambdaVariables)(tc)))(true)((() => {
  const typeLookup = LibMaps.lookup(name)(((_x) => _x.boundTypes)(tc));
  return LibMaybes.maybe((() => {
  const primLookup = LibMaps.lookup(name)(((_x) => _x.primitives)(tc));
  return LibMaybes.maybe(true)(((prim: Graph.Primitive) => LibEquality.gt(Arity.typeSchemeArity(((_x) => _x.type)(prim)))(0)))(primLookup);
})())(((ts: Core.TypeScheme) => LibEquality.gt(Arity.typeSchemeArity(ts))(0)))(typeLookup);
})()));
})());
}

export function isEncodedTerm(t: Core.Term): boolean {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "application": return ((a: Core.Application) => isEncodedTerm(((_x) => _x.function)(a)))((_m as any).value);
    case "inject": return ((i: Core.Injection) => LibEquality.equal("hydra.core.Term")(((_x) => _x)(((_x) => _x.typeName)(i))))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isEncodedType(t: Core.Term): boolean {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "application": return ((a: Core.Application) => isEncodedType(((_x) => _x.function)(a)))((_m as any).value);
    case "inject": return ((i: Core.Injection) => LibEquality.equal("hydra.core.Type")(((_x) => _x)(((_x) => _x.typeName)(i))))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isEnumRowType(rt: ReadonlyArray<Core.FieldType>): boolean {
  return LibLists.foldl(LibLogic.and)(true)(LibLists.map(((f: Core.FieldType) => isUnitType(Strip.deannotateType(((_x) => _x.type)(f)))))(rt));
}

export function isEnumType(typ: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => isEnumRowType(rt))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isNominalType(typ: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => true)((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => true)((_m as any).value);
    case "wrap": return ((wt: Core.Type) => true)((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => isNominalType(((_x) => _x.body)(fa)))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isSerializable(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Binding) => Errors.Error | boolean)) {
  return ((graph: Graph.Graph) => ((el: Core.Binding) => (() => {
  const variants = ((typ: Core.Type) => LibLists.map(Reflect.typeVariant)(Rewriting.foldOverType(({ tag: "pre" }))(((m: ReadonlyArray<Core.Type>) => ((t: Core.Type) => LibLists.cons(t)(m))))([])(typ)));
  return LibEithers.map(((deps: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const allVariants = LibSets.fromList(LibLists.concat(LibLists.map(variants)(LibMaps.elems(deps))));
  return LibLogic.not(LibSets.member(({ tag: "function" }))(allVariants));
})()))(typeDependencies(cx)(graph)(false)(LibEquality.identity)(((_x) => _x.name)(el)));
})()));
}

export function isSerializableByName(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Name) => Errors.Error | boolean)) {
  return ((graph: Graph.Graph) => ((name: Core.Name) => (() => {
  const variants = ((typ: Core.Type) => LibLists.map(Reflect.typeVariant)(Rewriting.foldOverType(({ tag: "pre" }))(((m: ReadonlyArray<Core.Type>) => ((t: Core.Type) => LibLists.cons(t)(m))))([])(typ)));
  return LibEithers.map(((deps: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const allVariants = LibSets.fromList(LibLists.concat(LibLists.map(variants)(LibMaps.elems(deps))));
  return LibLogic.not(LibSets.member(({ tag: "function" }))(allVariants));
})()))(typeDependencies(cx)(graph)(false)(LibEquality.identity)(name));
})()));
}

export function isSerializableType(typ: Core.Type): boolean {
  return (() => {
  const allVariants = LibSets.fromList(LibLists.map(Reflect.typeVariant)(Rewriting.foldOverType(({ tag: "pre" }))(((m: ReadonlyArray<Core.Type>) => ((t: Core.Type) => LibLists.cons(t)(m))))([])(typ)));
  return LibLogic.not(LibSets.member(({ tag: "function" }))(allVariants));
})();
}

export function isTrivialTerm(t: Core.Term): boolean {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "literal": return ((_: Core.Literal) => true)((_m as any).value);
    case "variable": return ((nm: Core.Name) => LibEquality.equal(LibLists.length(LibStrings.splitOn(".")(((_x) => _x)(nm))))(1))((_m as any).value);
    case "unit": return ((_: void) => true)((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  return (() => {
  const arg = ((_x) => _x.argument)(app);
  return (() => {
  const _m = fun;
  switch (_m.tag) {
    case "project": return ((_: Core.Projection) => isTrivialTerm(arg))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => isTrivialTerm(arg))((_m as any).value);
    default: return false(_m);
  }
})();
})();
})())((_m as any).value);
    case "maybe": return ((opt: Core.Term | null) => LibMaybes.maybe(true)(((inner: Core.Term) => isTrivialTerm(inner)))(opt))((_m as any).value);
    case "record": return ((rec: Core.Record) => LibLists.foldl(((acc: boolean) => ((fld: Core.Field) => LibLogic.and(acc)(isTrivialTerm(((_x) => _x.term)(fld))))))(true)(((_x) => _x.fields)(rec)))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => isTrivialTerm(((_x) => _x.body)(wt)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => isTrivialTerm(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => isTrivialTerm(((_x) => _x.body)(tl)))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isType(t: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "application": return ((a: Core.ApplicationType) => isType(((_x) => _x.function)(a)))((_m as any).value);
    case "forall": return ((l: Core.ForallType) => isType(((_x) => _x.body)(l)))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => false)((_m as any).value);
    case "variable": return ((v: Core.Name) => LibEquality.equal(v)("hydra.core.Type"))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isUnitTerm(v1: Core.Term): boolean {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isUnitType(v1: Core.Type): boolean {
  return (() => {
  const _m = v1;
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
}

export function typeDependencies(cx: Context.Context): ((x: Graph.Graph) => ((x: boolean) => ((x: ((x: Core.Type) => Core.Type)) => ((x: Core.Name) => Errors.Error | ReadonlyMap<Core.Name, Core.Type>)))) {
  return ((graph: Graph.Graph) => ((withSchema: boolean) => ((transform: ((x: Core.Type) => Core.Type)) => ((name: Core.Name) => (() => {
  const requireType = ((name2: Core.Name) => (() => {
  const cx1 = ({
    trace: LibLists.cons(LibStrings.cat2("type dependencies of ")(((_x) => _x)(name2)))(((_x) => _x.trace)(cx)),
    messages: ((_x) => _x.messages)(cx),
    other: ((_x) => _x.other)(cx)
  });
  return LibEithers.bind(Lexical.requireBinding(graph)(name2))(((el: Core.Binding) => LibEithers.bimap(((_e: Errors.DecodingError) => ({ tag: "decoding", value: _e })))(((_a: Core.Type) => _a))(DecodeCore.type(graph)(((_x) => _x.term)(el)))));
})());
  return (() => {
  const toPair = ((name2: Core.Name) => LibEithers.map(((typ: Core.Type) => [name2, transform(typ)]))(requireType(name2)));
  return (() => {
  const deps = ((seeds: ReadonlySet<Core.Name>) => ((names: ReadonlyMap<Core.Name, Core.Type>) => LibLogic.ifElse(LibSets.null_(seeds))(({ tag: "right", value: names }))(LibEithers.bind(LibEithers.mapList(toPair)(LibSets.toList(seeds)))(((pairs: ReadonlyArray<readonly [Core.Name, Core.Type]>) => (() => {
  const newNames = LibMaps.union(names)(LibMaps.fromList(pairs));
  return (() => {
  const refs = LibLists.foldl(LibSets.union)(LibSets.empty)(LibLists.map(((pair: readonly [Core.Name, Core.Type]) => Dependencies.typeDependencyNames(withSchema)(LibPairs.second(pair))))(pairs));
  return (() => {
  const visited = LibSets.fromList(LibMaps.keys(names));
  return (() => {
  const newSeeds = LibSets.difference(refs)(visited);
  return deps(newSeeds)(newNames);
})();
})();
})();
})())))));
  return deps(LibSets.singleton(name))(LibMaps.empty);
})();
})();
})()))));
}
