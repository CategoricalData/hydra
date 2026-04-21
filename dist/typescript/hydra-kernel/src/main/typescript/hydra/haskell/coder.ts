// Note: this is an automatically generated file. Do not edit.

/**
 * Functions for encoding Hydra modules as Haskell modules
 */



import * as Adapt from "../adapt.js";
import * as Analysis from "../analysis.js";
import * as Annotations from "../annotations.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Dependencies from "../dependencies.js";
import * as EncodeCore from "../encode/core.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as HaskellEnvironment from "./environment.js";
import * as HaskellLanguage from "./language.js";
import * as HaskellSerde from "./serde.js";
import * as HaskellSyntax from "./syntax.js";
import * as HaskellUtils from "./utils.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
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
import * as Predicates from "../predicates.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
import * as Resolution from "../resolution.js";
import * as Rewriting from "../rewriting.js";
import * as Serialization from "../serialization.js";
import * as ShowCore from "../show/core.js";
import * as ShowErrors from "../show/errors.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variables from "../variables.js";
import * as Variants from "../variants.js";

export function adaptTypeToHaskellAndEncode<t0, t1>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Type) => ((x: t0) => ((x: t1) => Errors.Error | HaskellSyntax.Type))) {
  return ((typ: Core.Type) => ((cx: t0) => ((g: t1) => (() => {
  const enc = ((t: Core.Type) => encodeType(namespaces)(t)(cx)(g));
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "variable": return ((_: Core.Name) => enc(typ))((_m as any).value);
    default: return LibEithers.bind(Adapt.adaptTypeForLanguage(HaskellLanguage.haskellLanguage)(typ))(((adaptedType: Core.Type) => enc(adaptedType)))(_m);
  }
})();
})())));
}

export function constantForFieldName(tname: Core.Name): ((x: Core.Name) => string) {
  return ((fname: Core.Name) => LibStrings.cat(["_", Names.localNameOf(tname), "_", ((_x) => _x)(fname)]));
}

export function constantForTypeName(tname: Core.Name): string {
  return LibStrings.cat2("_")(Names.localNameOf(tname));
}

export function constructModule(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Packaging.Module) => ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Module)))) {
  return ((mod: Packaging.Module) => ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const h = ((namespace: Packaging.Namespace) => ((_x) => _x)(namespace));
  const createDeclarations = ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "type": return ((type: Packaging.TypeDefinition) => (() => {
  const name = ((_x) => _x.name)(type);
  const typ = ((_x) => _x.type)(((_x) => _x.type)(type));
  return toTypeDeclarationsFrom(namespaces)(name)(typ)(cx)(g);
})())((_m as any).value);
    case "term": return ((term: Packaging.TermDefinition) => LibEithers.bind(toDataDeclaration(namespaces)(term)(cx)(g))(((d: HaskellSyntax.DeclarationWithComments) => ({ tag: "right", value: [d] }))))((_m as any).value);
  }
})());
  const importName = ((name: string) => LibStrings.intercalate(".")(LibLists.map(Formatting.capitalize)(LibStrings.splitOn(".")(name))));
  const imports = LibLists.concat2(domainImports)(standardImports);
  const domainImports = (() => {
  const toImport = ((pair: readonly [Packaging.Namespace, HaskellSyntax.ModuleName]) => (() => {
  const namespace = LibPairs.first(pair);
  const alias = LibPairs.second(pair);
  const name = h(namespace);
  return ({
    qualified: true,
    module: importName(name),
    as: alias,
    spec: null
  });
})());
  return LibLists.map(toImport)(LibMaps.toList(((_x) => _x.mapping)(namespaces)));
})();
  const meta = gatherMetadata(defs);
  const condImport = ((flag: boolean) => ((triple: t0) => LibLogic.ifElse(flag)([triple])([])));
  const standardImports = (() => {
  const toImport = ((triple: readonly [readonly [string, string | null], ReadonlyArray<string>]) => (() => {
  const name = LibPairs.first(LibPairs.first(triple));
  const malias = LibPairs.second(LibPairs.first(triple));
  const hidden = LibPairs.second(triple);
  const spec = LibLogic.ifElse(LibLists.null_(hidden))(null)(({ tag: "hiding", value: LibLists.map(((n: string) => ({
    modifier: null,
    name: HaskellUtils.simpleName(n),
    subspec: null
  })))(hidden) }));
  return ({
    qualified: LibMaybes.isJust(malias),
    module: name,
    as: LibMaybes.map(((x: string) => x))(malias),
    spec: spec
  });
})());
  return LibLists.map(toImport)(LibLists.concat([[[["Prelude", null], ["Enum", "Ordering", "decodeFloat", "encodeFloat", "fail", "map", "pure", "sum"]]], condImport(((_x) => _x.usesByteString)(meta))([["Data.ByteString", "B"], []]), condImport(((_x) => _x.usesInt)(meta))([["Data.Int", "I"], []]), condImport(((_x) => _x.usesMap)(meta))([["Data.Map", "M"], []]), condImport(((_x) => _x.usesSet)(meta))([["Data.Set", "S"], []]), LibLogic.ifElse(Analysis.moduleContainsBinaryLiterals(mod))([[["Hydra.Lib.Literals", "Literals"], []]])([])]));
})();
  return LibEithers.bind(LibEithers.mapList(createDeclarations)(defs))(((declLists: ReadonlyArray<ReadonlyArray<HaskellSyntax.DeclarationWithComments>>) => (() => {
  const decls = LibLists.concat(declLists);
  const mc = ((_x) => _x.description)(mod);
  return ({ tag: "right", value: ({
    head: ({
    comments: mc,
    name: importName(h(((_x) => _x.namespace)(mod))),
    exports: []
  }),
    imports: imports,
    declarations: decls
  }) });
})()));
})()))));
}

export const emptyMetadata: HaskellEnvironment.HaskellModuleMetadata = ({
    usesByteString: false,
    usesInt: false,
    usesMap: false,
    usesSet: false
  });

export function encodeCaseExpression<t0>(depth: number): ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((x: Core.CaseStatement) => ((x: HaskellSyntax.Expression) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Expression))))) {
  return ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((stmt: Core.CaseStatement) => ((scrutinee: HaskellSyntax.Expression) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const dn = ((_x) => _x.typeName)(stmt);
  const def = ((_x) => _x.default)(stmt);
  const fields = ((_x) => _x.cases)(stmt);
  const toAlt = ((fieldMap: ReadonlyMap<Core.Name, Core.FieldType>) => ((field: Core.Field) => (() => {
  const fn = ((_x) => _x.name)(field);
  const fun_ = ((_x) => _x.term)(field);
  const v0 = LibStrings.cat2("v")(LibLiterals.showInt32(depth));
  const raw = ({ tag: "application", value: ({
    function: fun_,
    argument: ({ tag: "variable", value: v0 })
  }) });
  const rhsTerm = Dependencies.simplifyTerm(raw);
  const v1 = LibLogic.ifElse(Variables.isFreeVariableInTerm(v0)(rhsTerm))(Constants.ignoredVariable)(v0);
  const hname = HaskellUtils.unionFieldReference(LibSets.union(LibSets.fromList(LibMaps.keys(((_x) => _x.boundTerms)(g))))(LibSets.fromList(LibMaps.keys(((_x) => _x.schemaTypes)(g)))))(namespaces)(dn)(fn);
  return LibEithers.bind(LibMaybes.cases(LibMaps.lookup(fn)(fieldMap))(({ tag: "left", value: ({ tag: "resolution", value: ({ tag: "noMatchingField", value: ({
    fieldName: fn
  }) }) }) }))(((fieldType: Core.FieldType) => (() => {
  const ft = ((_x) => _x.type)(fieldType);
  const noArgs = [];
  const singleArg = [({ tag: "name", value: HaskellUtils.rawName(v1) })];
  return (() => {
  const _m = Strip.deannotateType(ft);
  switch (_m.tag) {
    case "unit": return ((_: void) => ({ tag: "right", value: noArgs }))((_m as any).value);
    default: return ({ tag: "right", value: singleArg })(_m);
  }
})();
})())))(((args: ReadonlyArray<HaskellSyntax.Pattern>) => (() => {
  const lhs = HaskellUtils.applicationPattern(hname)(args);
  return LibEithers.bind(LibEithers.map(((x: HaskellSyntax.Expression) => x))(encodeTerm(LibMath.add(depth)(1))(namespaces)(rhsTerm)(cx)(g)))(((rhs: HaskellSyntax.CaseRhs) => ({ tag: "right", value: ({
    pattern: lhs,
    rhs: rhs,
    binds: null
  }) })));
})()));
})()));
  return LibEithers.bind(Resolution.requireUnionType(cx)(g)(dn))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const toFieldMapEntry = ((f: Core.FieldType) => [((_x) => _x.name)(f), f]);
  const fieldMap = LibMaps.fromList(LibLists.map(toFieldMapEntry)(rt));
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => toAlt(fieldMap)(v1)))(fields))(((ecases: ReadonlyArray<HaskellSyntax.Alternative>) => LibEithers.bind(LibMaybes.cases(def)(({ tag: "right", value: [] }))(((d: Core.Term) => LibEithers.bind(LibEithers.map(((x: HaskellSyntax.Expression) => x))(encodeTerm(depth)(namespaces)(d)(cx)(g)))(((cs: HaskellSyntax.CaseRhs) => (() => {
  const lhs = ({ tag: "name", value: HaskellUtils.rawName(Constants.ignoredVariable) });
  const alt = ({
    pattern: lhs,
    rhs: cs,
    binds: null
  });
  return ({ tag: "right", value: [alt] });
})())))))(((dcases: ReadonlyArray<HaskellSyntax.Alternative>) => ({ tag: "right", value: ({ tag: "case", value: ({
    case: scrutinee,
    alternatives: LibLists.concat2(ecases)(dcases)
  }) }) })))));
})()));
})())))));
}

export function encodeLambdaTerm<t0>(depth: number): ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((x: Core.Lambda) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Expression)))) {
  return ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((lam: Core.Lambda) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const v = ((_x) => _x.parameter)(lam);
  const body = ((_x) => _x.body)(lam);
  return LibEithers.bind(encodeTerm(depth)(namespaces)(body)(cx)(g))(((hbody: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hslambda(HaskellUtils.elementReference(namespaces)(v))(hbody) })));
})()))));
}

export function encodeLiteral<t0>(l: Core.Literal): ((x: t0) => Errors.Error | HaskellSyntax.Expression) {
  return ((cx: t0) => (() => {
  const _m = l;
  switch (_m.tag) {
    case "binary": return ((bs: Uint8Array) => ({ tag: "right", value: HaskellUtils.hsapp(HaskellUtils.hsvar("Literals.stringToBinary"))(HaskellUtils.hslit(({ tag: "string", value: LibLiterals.binaryToString(bs) }))) }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "right", value: HaskellUtils.hsvar(LibLogic.ifElse(b)("True")("False")) }))((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "float32": return ((f: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "float", value: f })) }))((_m as any).value);
    case "float64": return ((f: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "double", value: f })) }))((_m as any).value);
    case "bigfloat": return ((f: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "double", value: LibLiterals.bigfloatToFloat64(f) })) }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: i })) }))((_m as any).value);
    case "int8": return ((i: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.int8ToBigint(i) })) }))((_m as any).value);
    case "int16": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.int16ToBigint(i) })) }))((_m as any).value);
    case "int32": return ((i: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "int", value: i })) }))((_m as any).value);
    case "int64": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.int64ToBigint(i) })) }))((_m as any).value);
    case "uint8": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.uint8ToBigint(i) })) }))((_m as any).value);
    case "uint16": return ((i: number) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.uint16ToBigint(i) })) }))((_m as any).value);
    case "uint32": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.uint32ToBigint(i) })) }))((_m as any).value);
    case "uint64": return ((i: bigint) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "integer", value: LibLiterals.uint64ToBigint(i) })) }))((_m as any).value);
  }
})())((_m as any).value);
    case "string": return ((s: string) => ({ tag: "right", value: HaskellUtils.hslit(({ tag: "string", value: s })) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "supported literal",
    actual: ShowCore.literal(l)
  }) }) }) })(_m);
  }
})());
}

export function encodeProjection<t0>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Projection) => t0 | HaskellSyntax.Expression) {
  return ((proj: Core.Projection) => (() => {
  const dn = ((_x) => _x.typeName)(proj);
  const fname = ((_x) => _x.field)(proj);
  return ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.recordFieldReference(namespaces)(dn)(fname) }) });
})());
}

export function encodeStandaloneCases<t0>(depth: number): ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((x: Core.CaseStatement) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Expression)))) {
  return ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((stmt: Core.CaseStatement) => ((cx: t0) => ((g: Graph.Graph) => LibEithers.map(((v1: HaskellSyntax.Expression) => HaskellUtils.hslambda(HaskellUtils.rawName("x"))(v1)))(encodeCaseExpression(depth)(namespaces)(stmt)(HaskellUtils.hsvar("x"))(cx)(g))))));
}

export function encodeTerm<t0>(depth: number): ((x: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((x: Core.Term) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Expression)))) {
  return ((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => ((term: Core.Term) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const encode = ((t: Core.Term) => encodeTerm(depth)(namespaces)(t)(cx)(g));
  return (() => {
  const nonemptyMap = ((m: ReadonlyMap<Core.Term, Core.Term>) => (() => {
  const lhs = HaskellUtils.hsvar("M.fromList");
  const encodePair = ((pair: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(pair);
  const v = LibPairs.second(pair);
  return LibEithers.bind(encode(k))(((hk: HaskellSyntax.Expression) => LibEithers.bind(encode(v))(((hv: HaskellSyntax.Expression) => ({ tag: "right", value: ({ tag: "tuple", value: [hk, hv] }) })))));
})());
  return LibEithers.bind(LibEithers.map(((x: ReadonlyArray<HaskellSyntax.Expression>) => ({ tag: "list", value: x })))(LibEithers.mapList(encodePair)(LibMaps.toList(m))))(((rhs: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(lhs)(rhs) })));
})());
  return (() => {
  const nonemptySet = ((s: ReadonlySet<Core.Term>) => (() => {
  const lhs = HaskellUtils.hsvar("S.fromList");
  return LibEithers.bind(encodeTerm(depth)(namespaces)(({ tag: "list", value: LibSets.toList(s) }))(cx)(g))(((rhs: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(lhs)(rhs) })));
})());
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  const arg = ((_x) => _x.argument)(app);
  const deannotatedFun = Strip.deannotateTerm(fun);
  return (() => {
  const _m = deannotatedFun;
  switch (_m.tag) {
    case "cases": return ((stmt: Core.CaseStatement) => LibEithers.bind(encode(arg))(((harg: HaskellSyntax.Expression) => encodeCaseExpression(depth)(namespaces)(stmt)(harg)(cx)(g))))((_m as any).value);
    default: return LibEithers.bind(encode(fun))(((hfun: HaskellSyntax.Expression) => LibEithers.bind(encode(arg))(((harg: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(hfun)(harg) })))))(_m);
  }
})();
})())((_m as any).value);
    case "cases": return ((stmt: Core.CaseStatement) => encodeStandaloneCases(depth)(namespaces)(stmt)(cx)(g))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibEithers.bind(encode(l))(((hl: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(HaskellUtils.hsvar("Left"))(hl) })))))(((r: Core.Term) => LibEithers.bind(encode(r))(((hr: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(HaskellUtils.hsvar("Right"))(hr) })))))(e))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => encodeLambdaTerm(depth)(namespaces)(lam)(cx)(g))((_m as any).value);
    case "project": return ((proj: Core.Projection) => encodeProjection(namespaces)(proj))((_m as any).value);
    case "unwrap": return ((name: Core.Name) => encodeUnwrap(namespaces)(name))((_m as any).value);
    case "let": return ((letTerm: Core.Let) => (() => {
  const collectBindings = ((lt: Core.Let) => (() => {
  const bs = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return (() => {
  const _m = Strip.deannotateTerm(body);
  switch (_m.tag) {
    case "let": return ((innerLt: Core.Let) => (() => {
  const innerResult = collectBindings(innerLt);
  return [LibLists.concat2(bs)(LibPairs.first(innerResult)), LibPairs.second(innerResult)];
})())((_m as any).value);
    default: return [bs, body](_m);
  }
})();
})();
})());
  const collected = collectBindings(letTerm);
  const allBindings = LibPairs.first(collected);
  const finalBody = LibPairs.second(collected);
  const encodeBinding = ((binding: Core.Binding) => (() => {
  const name = ((_x) => _x.name)(binding);
  const term_ = ((_x) => _x.term)(binding);
  const hname = HaskellUtils.simpleName(((_x) => _x)(name));
  return LibEithers.bind(encode(term_))(((hexpr: HaskellSyntax.Expression) => ({ tag: "right", value: ({ tag: "value", value: HaskellUtils.simpleValueBinding(hname)(hexpr)(null) }) })));
})());
  return LibEithers.bind(LibEithers.mapList(encodeBinding)(allBindings))(((hbindings: ReadonlyArray<HaskellSyntax.LocalBinding>) => LibEithers.bind(encode(finalBody))(((hinner: HaskellSyntax.Expression) => ({ tag: "right", value: ({ tag: "let", value: ({
    bindings: hbindings,
    inner: hinner
  }) }) })))));
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(encode)(els))(((helems: ReadonlyArray<HaskellSyntax.Expression>) => ({ tag: "right", value: ({ tag: "list", value: helems }) }))))((_m as any).value);
    case "literal": return ((v: Core.Literal) => encodeLiteral(v)(cx))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLogic.ifElse(LibMaps.null_(m))(({ tag: "right", value: HaskellUtils.hsvar("M.empty") }))(nonemptyMap(m)))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.cases(m)(({ tag: "right", value: HaskellUtils.hsvar("Nothing") }))(((t: Core.Term) => LibEithers.bind(encode(t))(((ht: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(HaskellUtils.hsvar("Just"))(ht) }))))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(encode(LibPairs.first(p)))(((f: HaskellSyntax.Expression) => LibEithers.bind(encode(LibPairs.second(p)))(((s: HaskellSyntax.Expression) => ({ tag: "right", value: ({ tag: "tuple", value: [f, s] }) }))))))((_m as any).value);
    case "record": return ((record: Core.Record) => (() => {
  const sname = ((_x) => _x.typeName)(record);
  const fields = ((_x) => _x.fields)(record);
  const toFieldUpdate = ((field: Core.Field) => (() => {
  const fn = ((_x) => _x.name)(field);
  const ft = ((_x) => _x.term)(field);
  const fieldRef = HaskellUtils.recordFieldReference(namespaces)(sname)(fn);
  return LibEithers.bind(encode(ft))(((hft: HaskellSyntax.Expression) => ({ tag: "right", value: ({
    name: fieldRef,
    value: hft
  }) })));
})());
  const typeName = HaskellUtils.elementReference(namespaces)(sname);
  return LibEithers.bind(LibEithers.mapList(toFieldUpdate)(fields))(((updates: ReadonlyArray<HaskellSyntax.FieldUpdate>) => ({ tag: "right", value: ({ tag: "constructRecord", value: ({
    name: typeName,
    fields: updates
  }) }) })));
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibLogic.ifElse(LibSets.null_(s))(({ tag: "right", value: HaskellUtils.hsvar("S.empty") }))(nonemptySet(s)))((_m as any).value);
    case "typeLambda": return ((abs: Core.TypeLambda) => (() => {
  const term1 = ((_x) => _x.body)(abs);
  return encode(term1);
})())((_m as any).value);
    case "typeApplication": return ((typed: Core.TypeApplicationTerm) => (() => {
  const term1 = ((_x) => _x.body)(typed);
  return encode(term1);
})())((_m as any).value);
    case "inject": return ((injection: Core.Injection) => (() => {
  const sname = ((_x) => _x.typeName)(injection);
  const field = ((_x) => _x.field)(injection);
  const fn = ((_x) => _x.name)(field);
  const ft = ((_x) => _x.term)(field);
  const lhs = ({ tag: "variable", value: HaskellUtils.unionFieldReference(LibSets.union(LibSets.fromList(LibMaps.keys(((_x) => _x.boundTerms)(g))))(LibSets.fromList(LibMaps.keys(((_x) => _x.schemaTypes)(g)))))(namespaces)(sname)(fn) });
  const dflt = LibEithers.map(((v1: HaskellSyntax.Expression) => HaskellUtils.hsapp(lhs)(v1)))(encode(ft));
  return LibEithers.bind(Resolution.requireUnionField(cx)(g)(sname)(fn))(((ftyp: Core.Type) => (() => {
  const _m = Strip.deannotateType(ftyp);
  switch (_m.tag) {
    case "unit": return ((_: void) => ({ tag: "right", value: lhs }))((_m as any).value);
    default: return dflt(_m);
  }
})()));
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "tuple", value: [] }) }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.elementReference(namespaces)(name) }) }))((_m as any).value);
    case "wrap": return ((wrapped: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x.typeName)(wrapped);
  const term_ = ((_x) => _x.body)(wrapped);
  const lhs = ({ tag: "variable", value: HaskellUtils.elementReference(namespaces)(tname) });
  return LibEithers.bind(encode(term_))(((rhs: HaskellSyntax.Expression) => ({ tag: "right", value: HaskellUtils.hsapp(lhs)(rhs) })));
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "supported term",
    actual: ShowCore.term(term)
  }) }) }) })(_m);
  }
})();
})();
})();
})()))));
}

export function encodeType<t0, t1>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Type) => ((x: t0) => ((x: t1) => Errors.Error | HaskellSyntax.Type))) {
  return ((typ: Core.Type) => ((cx: t0) => ((g: t1) => (() => {
  const encode = ((t: Core.Type) => encodeType(namespaces)(t)(cx)(g));
  const ref = ((name: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.elementReference(namespaces)(name) }) }));
  const unitTuple = ({ tag: "tuple", value: [] });
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "application": return ((app: Core.ApplicationType) => (() => {
  const lhs = ((_x) => _x.function)(app);
  const rhs = ((_x) => _x.argument)(app);
  return LibEithers.bind(encode(lhs))(((hlhs: HaskellSyntax.Type) => LibEithers.bind(encode(rhs))(((hrhs: HaskellSyntax.Type) => ({ tag: "right", value: HaskellUtils.toTypeApplication([hlhs, hrhs]) })))));
})())((_m as any).value);
    case "either": return ((eitherType: Core.EitherType) => (() => {
  const left_ = ((_x) => _x.left)(eitherType);
  const right_ = ((_x) => _x.right)(eitherType);
  return LibEithers.bind(encode(left_))(((hleft: HaskellSyntax.Type) => LibEithers.bind(encode(right_))(((hright: HaskellSyntax.Type) => ({ tag: "right", value: HaskellUtils.toTypeApplication([({ tag: "variable", value: HaskellUtils.rawName("Either") }), hleft, hright]) })))));
})())((_m as any).value);
    case "function": return ((funType: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(funType);
  const cod = ((_x) => _x.codomain)(funType);
  return LibEithers.bind(encode(dom))(((hdom: HaskellSyntax.Type) => LibEithers.bind(encode(cod))(((hcod: HaskellSyntax.Type) => ({ tag: "right", value: ({ tag: "function", value: ({
    domain: hdom,
    codomain: hcod
  }) }) })))));
})())((_m as any).value);
    case "forall": return ((forallType: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(forallType);
  const body = ((_x) => _x.body)(forallType);
  return encode(body);
})())((_m as any).value);
    case "list": return ((lt: Core.Type) => LibEithers.bind(encode(lt))(((hlt: HaskellSyntax.Type) => ({ tag: "right", value: ({ tag: "list", value: hlt }) }))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("B.ByteString") }) }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Bool") }) }))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "float32": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Float") }) }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Double") }) }))((_m as any).value);
    case "bigfloat": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Double") }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Integer") }) }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("I.Int8") }) }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("I.Int16") }) }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Int") }) }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("I.Int64") }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "supported integer type",
    actual: ShowCore.integerType(it)
  }) }) }) })(_m);
  }
})())((_m as any).value);
    case "string": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("String") }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "supported literal type",
    actual: ShowCore.literalType(lt)
  }) }) }) })(_m);
  }
})())((_m as any).value);
    case "map": return ((mapType: Core.MapType) => (() => {
  const kt = ((_x) => _x.keys)(mapType);
  const vt = ((_x) => _x.values)(mapType);
  return LibEithers.bind(encode(kt))(((hkt: HaskellSyntax.Type) => LibEithers.bind(encode(vt))(((hvt: HaskellSyntax.Type) => ({ tag: "right", value: HaskellUtils.toTypeApplication([({ tag: "variable", value: HaskellUtils.rawName("M.Map") }), hkt, hvt]) })))));
})())((_m as any).value);
    case "maybe": return ((ot: Core.Type) => LibEithers.bind(encode(ot))(((hot: HaskellSyntax.Type) => ({ tag: "right", value: HaskellUtils.toTypeApplication([({ tag: "variable", value: HaskellUtils.rawName("Maybe") }), hot]) }))))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibEithers.bind(encode(((_x) => _x.first)(pt)))(((f: HaskellSyntax.Type) => LibEithers.bind(encode(((_x) => _x.second)(pt)))(((s: HaskellSyntax.Type) => ({ tag: "right", value: ({ tag: "tuple", value: [f, s] }) }))))))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ref("placeholder"))((_m as any).value);
    case "set": return ((st: Core.Type) => LibEithers.bind(encode(st))(((hst: HaskellSyntax.Type) => ({ tag: "right", value: HaskellUtils.toTypeApplication([({ tag: "variable", value: HaskellUtils.rawName("S.Set") }), hst]) }))))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ref("placeholder"))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: unitTuple }))((_m as any).value);
    case "variable": return ((v1: Core.Name) => ref(v1))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.rawName("Void") }) }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ref("placeholder"))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "supported type",
    actual: ShowCore.type(typ)
  }) }) }) })(_m);
  }
})();
})())));
}

export function encodeTypeWithClassAssertions<t0, t1>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>>) => ((x: Core.Type) => ((x: t0) => ((x: t1) => Errors.Error | HaskellSyntax.Type)))) {
  return ((explicitClasses: ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>>) => ((typ: Core.Type) => ((cx: t0) => ((g: t1) => (() => {
  const classes = LibMaps.union(explicitClasses)(getImplicitTypeClasses(typ));
  const implicitClasses = getImplicitTypeClasses(typ);
  const encodeAssertion = ((pair: readonly [Core.Name, Classes.TypeClass]) => (() => {
  const name = LibPairs.first(pair);
  const cls = LibPairs.second(pair);
  const hname = HaskellUtils.rawName((() => {
  const _m = cls;
  switch (_m.tag) {
    case "equality": return ((_: void) => "Eq")((_m as any).value);
    case "ordering": return ((_: void) => "Ord")((_m as any).value);
  }
})());
  const htype = ({ tag: "variable", value: HaskellUtils.rawName(((_x) => _x)(name)) });
  return ({ tag: "class", value: ({
    name: hname,
    types: [htype]
  }) });
})());
  const assertPairs = LibLists.concat(LibLists.map(toPairs)(LibMaps.toList(classes)));
  const toPairs = ((mapEntry: readonly [t2, ReadonlySet<t3>]) => (() => {
  const name = LibPairs.first(mapEntry);
  const clsSet = LibPairs.second(mapEntry);
  const toPair = ((c: t4) => [name, c]);
  return LibLists.map(toPair)(LibSets.toList(clsSet));
})());
  return LibEithers.bind(adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))(((htyp: HaskellSyntax.Type) => LibLogic.ifElse(LibLists.null_(assertPairs))(({ tag: "right", value: htyp }))((() => {
  const encoded = LibLists.map(encodeAssertion)(assertPairs);
  const hassert = LibLogic.ifElse(LibEquality.equal(LibLists.length(encoded))(1))(LibLists.head(encoded))(({ tag: "tuple", value: encoded }));
  return ({ tag: "right", value: ({ tag: "ctx", value: ({
    ctx: hassert,
    type: htyp
  }) }) });
})())));
})()))));
}

export function encodeUnwrap<t0>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => t0 | HaskellSyntax.Expression) {
  return ((name: Core.Name) => ({ tag: "right", value: ({ tag: "variable", value: HaskellUtils.elementReference(namespaces)(Names.qname(LibMaybes.fromJust(Names.namespaceOf(name)))(HaskellUtils.newtypeAccessorName(name))) }) }));
}

export function extendMetaForTerm(meta: HaskellEnvironment.HaskellModuleMetadata): ((x: Core.Term) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "map": return ((_: ReadonlyMap<Core.Term, Core.Term>) => setMetaUsesMap(true)(meta))((_m as any).value);
    case "set": return ((_: ReadonlySet<Core.Term>) => setMetaUsesSet(true)(meta))((_m as any).value);
    default: return meta(_m);
  }
})());
}

export function extendMetaForType(meta: HaskellEnvironment.HaskellModuleMetadata): ((x: Core.Type) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => setMetaUsesByteString(true)(meta))((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "int8": return ((_: void) => setMetaUsesInt(true)(meta))((_m as any).value);
    case "int16": return ((_: void) => setMetaUsesInt(true)(meta))((_m as any).value);
    case "int64": return ((_: void) => setMetaUsesInt(true)(meta))((_m as any).value);
    default: return meta(_m);
  }
})())((_m as any).value);
    default: return meta(_m);
  }
})())((_m as any).value);
    case "map": return ((_: Core.MapType) => setMetaUsesMap(true)(meta))((_m as any).value);
    case "set": return ((_: Core.Type) => setMetaUsesSet(true)(meta))((_m as any).value);
    default: return meta(_m);
  }
})());
}

export function findOrdVariables(typ: Core.Type): ReadonlySet<Core.Name> {
  return (() => {
  const fold = ((names: ReadonlySet<Core.Name>) => ((typ_: Core.Type) => (() => {
  const _m = typ_;
  switch (_m.tag) {
    case "map": return ((mapType: Core.MapType) => (() => {
  const kt = ((_x) => _x.keys)(mapType);
  return tryType(names)(kt);
})())((_m as any).value);
    case "set": return ((et: Core.Type) => tryType(names)(et))((_m as any).value);
    default: return names(_m);
  }
})()));
  const isTypeVariable = ((v: Core.Name) => LibMaybes.isNothing(Names.namespaceOf(v)));
  const tryType = ((names: ReadonlySet<Core.Name>) => ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(isTypeVariable(v))(LibSets.insert(v)(names))(names))((_m as any).value);
    default: return names(_m);
  }
})()));
  return Rewriting.foldOverType(({ tag: "pre" }))(fold)(LibSets.empty)(typ);
})();
}

export function gatherMetadata(defs: ReadonlyArray<Packaging.Definition>): HaskellEnvironment.HaskellModuleMetadata {
  return (() => {
  const addDef = ((meta: HaskellEnvironment.HaskellModuleMetadata) => ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "term": return ((termDef: Packaging.TermDefinition) => (() => {
  const term = ((_x) => _x.term)(termDef);
  return (() => {
  const metaWithTerm = Rewriting.foldOverTerm(({ tag: "pre" }))(((m: HaskellEnvironment.HaskellModuleMetadata) => ((t: Core.Term) => extendMetaForTerm(m)(t))))(meta)(term);
  return LibMaybes.maybe(metaWithTerm)(((ts: Core.TypeScheme) => Rewriting.foldOverType(({ tag: "pre" }))(((m: HaskellEnvironment.HaskellModuleMetadata) => ((t: Core.Type) => extendMetaForType(m)(t))))(metaWithTerm)(((_x) => _x.type)(ts))))(((_x) => _x.type)(termDef));
})();
})())((_m as any).value);
    case "type": return ((typeDef: Packaging.TypeDefinition) => (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(typeDef));
  return Rewriting.foldOverType(({ tag: "pre" }))(((m: HaskellEnvironment.HaskellModuleMetadata) => ((t: Core.Type) => extendMetaForType(m)(t))))(meta)(typ);
})())((_m as any).value);
  }
})()));
  return LibLists.foldl(addDef)(emptyMetadata)(defs);
})();
}

export function getImplicitTypeClasses(typ: Core.Type): ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>> {
  return (() => {
  const toPair = ((name: t0) => [name, LibSets.fromList([({ tag: "ordering" })])]);
  return LibMaps.fromList(LibLists.map(toPair)(LibSets.toList(findOrdVariables(typ))));
})();
}

export const includeTypeDefinitions: boolean = false;

export const keyHaskellVar: Core.Name = "haskellVar";

export function moduleToHaskell(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<string, string>))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(moduleToHaskellModule(mod)(defs)(cx)(g))(((hsmod: HaskellSyntax.Module) => (() => {
  const s = Serialization.printExpr(Serialization.parenthesize(HaskellSerde.moduleToExpr(hsmod)));
  const filepath = Names.namespaceToFilePath(({ tag: "pascal" }))("hs")(((_x) => _x.namespace)(mod));
  return ({ tag: "right", value: LibMaps.singleton(filepath)(s) });
})())))));
}

export function moduleToHaskellModule(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.Module))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(HaskellUtils.namespacesForModule(mod)(cx)(g))(((namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>) => constructModule(namespaces)(mod)(defs)(cx)(g))))));
}

export function nameDecls(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => ((x: Core.Type) => ReadonlyArray<HaskellSyntax.DeclarationWithComments>)) {
  return ((name: Core.Name) => ((typ: Core.Type) => (() => {
  const nm = ((_x) => _x)(name);
  const toDecl = ((n: Core.Name) => ((pair: readonly [string, string]) => (() => {
  const k = LibPairs.first(pair);
  const v = LibPairs.second(pair);
  const decl = ({ tag: "valueBinding", value: ({ tag: "simple", value: ({
    pattern: HaskellUtils.applicationPattern(HaskellUtils.simpleName(k))([]),
    rhs: ({ tag: "application", value: ({
    function: ({ tag: "variable", value: HaskellUtils.elementReference(namespaces)(n) }),
    argument: ({ tag: "literal", value: ({ tag: "string", value: v }) })
  }) }),
    localBindings: null
  }) }) });
  return ({
    body: decl,
    comments: null
  });
})()));
  const nameDecl = [constantForTypeName(name), nm];
  const fieldDecls = LibLists.map(toConstant)(Lexical.fieldsOf(typ));
  const toConstant = ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  return [constantForFieldName(name)(fname), ((_x) => _x)(fname)];
})());
  return LibLogic.ifElse(useCoreImport)(LibLists.cons(toDecl("hydra.core.Name")(nameDecl))(LibLists.map(((v1: readonly [string, string]) => toDecl("hydra.core.Name")(v1)))(fieldDecls)))([]);
})()));
}

export function setMetaUsesByteString(b: boolean): ((x: HaskellEnvironment.HaskellModuleMetadata) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((m: HaskellEnvironment.HaskellModuleMetadata) => ({
    usesByteString: b,
    usesInt: ((_x) => _x.usesInt)(m),
    usesMap: ((_x) => _x.usesMap)(m),
    usesSet: ((_x) => _x.usesSet)(m)
  }));
}

export function setMetaUsesInt(b: boolean): ((x: HaskellEnvironment.HaskellModuleMetadata) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((m: HaskellEnvironment.HaskellModuleMetadata) => ({
    usesByteString: ((_x) => _x.usesByteString)(m),
    usesInt: b,
    usesMap: ((_x) => _x.usesMap)(m),
    usesSet: ((_x) => _x.usesSet)(m)
  }));
}

export function setMetaUsesMap(b: boolean): ((x: HaskellEnvironment.HaskellModuleMetadata) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((m: HaskellEnvironment.HaskellModuleMetadata) => ({
    usesByteString: ((_x) => _x.usesByteString)(m),
    usesInt: ((_x) => _x.usesInt)(m),
    usesMap: b,
    usesSet: ((_x) => _x.usesSet)(m)
  }));
}

export function setMetaUsesSet(b: boolean): ((x: HaskellEnvironment.HaskellModuleMetadata) => HaskellEnvironment.HaskellModuleMetadata) {
  return ((m: HaskellEnvironment.HaskellModuleMetadata) => ({
    usesByteString: ((_x) => _x.usesByteString)(m),
    usesInt: ((_x) => _x.usesInt)(m),
    usesMap: ((_x) => _x.usesMap)(m),
    usesSet: b
  }));
}

export function toDataDeclaration<t0>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Packaging.TermDefinition) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.DeclarationWithComments))) {
  return ((def: Packaging.TermDefinition) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const name = ((_x) => _x.name)(def);
  const term = ((_x) => _x.term)(def);
  const typ = ((_x) => _x.type)(def);
  const hname = HaskellUtils.simpleName(Names.localNameOf(name));
  const rewriteValueBinding = ((vb: HaskellSyntax.ValueBinding) => (() => {
  const _m = vb;
  switch (_m.tag) {
    case "simple": return ((simple: HaskellSyntax.SimpleValueBinding) => (() => {
  const pattern_ = ((_x) => _x.pattern)(simple);
  const rhs = ((_x) => _x.rhs)(simple);
  const bindings = ((_x) => _x.localBindings)(simple);
  return (() => {
  const _m = pattern_;
  switch (_m.tag) {
    case "application": return ((appPat: HaskellSyntax.ApplicationPattern) => (() => {
  const name_ = ((_x) => _x.name)(appPat);
  const args = ((_x) => _x.args)(appPat);
  const rhsExpr = ((_x) => _x)(rhs);
  return (() => {
  const _m = rhsExpr;
  switch (_m.tag) {
    case "lambda": return ((lambda_: HaskellSyntax.LambdaExpression) => (() => {
  const vars = ((_x) => _x.bindings)(lambda_);
  const body = ((_x) => _x.inner)(lambda_);
  const newPattern = HaskellUtils.applicationPattern(name_)(LibLists.concat2(args)(vars));
  const newRhs = body;
  return rewriteValueBinding(({ tag: "simple", value: ({
    pattern: newPattern,
    rhs: newRhs,
    localBindings: bindings
  }) }));
})())((_m as any).value);
    default: return vb(_m);
  }
})();
})())((_m as any).value);
    default: return vb(_m);
  }
})();
})())((_m as any).value);
  }
})());
  const toDecl = ((comments: string | null) => ((hname_: HaskellSyntax.Name) => ((term_: Core.Term) => ((bindings: HaskellSyntax.LocalBindings | null) => (() => {
  const _m = Strip.deannotateTerm(term_);
  switch (_m.tag) {
    case "let": return ((letTerm: Core.Let) => (() => {
  const lbindings = ((_x) => _x.bindings)(letTerm);
  const env = ((_x) => _x.body)(letTerm);
  const toTermDefinition = ((hname__: HaskellSyntax.Name) => ((hterm_: HaskellSyntax.Expression) => ({ tag: "value", value: HaskellUtils.simpleValueBinding(hname__)(hterm_)(null) })));
  const hnames = LibLists.map(((binding: Core.Binding) => HaskellUtils.simpleName(((_x) => _x)(((_x) => _x.name)(binding)))))(lbindings);
  const terms = LibLists.map(((_x) => _x.term))(lbindings);
  return LibEithers.bind(LibEithers.mapList(((t: Core.Term) => encodeTerm(0)(namespaces)(t)(cx)(g)))(terms))(((hterms: ReadonlyArray<HaskellSyntax.Expression>) => (() => {
  const hbindings = LibLists.zipWith(toTermDefinition)(hnames)(hterms);
  const prevBindings = LibMaybes.maybe([])(((lb: HaskellSyntax.LocalBindings) => ((_x) => _x)(lb)))(bindings);
  const allBindings = LibLists.concat2(prevBindings)(hbindings);
  return toDecl(comments)(hname_)(env)(allBindings);
})()));
})())((_m as any).value);
    default: return LibEithers.bind(encodeTerm(0)(namespaces)(term_)(cx)(g))(((hterm: HaskellSyntax.Expression) => (() => {
  const vb = HaskellUtils.simpleValueBinding(hname_)(hterm)(bindings);
  const schemeConstraints = LibMaybes.maybe(null)(((ts: Core.TypeScheme) => ((_x) => _x.constraints)(ts)))(typ);
  const schemeClasses = typeSchemeConstraintsToClassMap(schemeConstraints);
  return LibEithers.bind(Annotations.getTypeClasses(cx)(g)(Strip.removeTypesFromTerm(term)))(((explicitClasses: ReadonlyMap<Core.Name, ReadonlySet<Classes.TypeClass>>) => (() => {
  const combinedClasses = LibMaps.union(schemeClasses)(explicitClasses);
  return (() => {
  const schemeType = LibMaybes.maybe(({ tag: "unit" }))(((ts: Core.TypeScheme) => ((_x) => _x.type)(ts)))(typ);
  return LibEithers.bind(encodeTypeWithClassAssertions(namespaces)(combinedClasses)(schemeType)(cx)(g))(((htype: HaskellSyntax.Type) => (() => {
  const decl = ({ tag: "typedBinding", value: ({
    typeSignature: ({
    name: hname_,
    type: htype
  }),
    valueBinding: rewriteValueBinding(vb)
  }) });
  return ({ tag: "right", value: ({
    body: decl,
    comments: comments
  }) });
})()));
})();
})()));
})()))(_m);
  }
})()))));
  return LibEithers.bind(Annotations.getTermDescription(cx)(g)(term))(((comments: string | null) => toDecl(comments)(hname)(term)(null)));
})())));
}

export function toTypeDeclarationsFrom(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => ((x: Core.Type) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<HaskellSyntax.DeclarationWithComments>)))) {
  return ((elementName: Core.Name) => ((typ: Core.Type) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const lname = Names.localNameOf(elementName);
  const hname = HaskellUtils.simpleName(lname);
  const declHead = ((name: HaskellSyntax.Name) => ((vars_: ReadonlyArray<Core.Name>) => LibLogic.ifElse(LibLists.null_(vars_))(({ tag: "simple", value: name }))((() => {
  const h = LibLists.head(vars_);
  const rest = LibLists.tail(vars_);
  const hvar = HaskellUtils.simpleName(((_x) => _x)(h));
  return ({ tag: "application", value: ({
    function: declHead(name)(rest),
    operand: hvar
  }) });
})())));
  const newtypeCons = ((tname: Core.Name) => ((typ_: Core.Type) => (() => {
  const hname0 = HaskellUtils.simpleName(HaskellUtils.newtypeAccessorName(tname));
  return LibEithers.bind(adaptTypeToHaskellAndEncode(namespaces)(typ_)(cx)(g))(((htype: HaskellSyntax.Type) => (() => {
  const hfield = ({
    field: ({
    name: hname0,
    type: htype
  }),
    comments: null
  });
  const constructorName = HaskellUtils.simpleName(Names.localNameOf(tname));
  return ({ tag: "right", value: ({
    body: ({ tag: "record", value: ({
    name: constructorName,
    fields: [hfield]
  }) }),
    comments: null
  }) });
})()));
})()));
  const recordCons = ((lname_: string) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const toField = ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  const ftype = ((_x) => _x.type)(fieldType);
  const hname_ = HaskellUtils.simpleName(LibStrings.cat2(Formatting.decapitalize(lname_))(Formatting.capitalize(((_x) => _x)(fname))));
  return LibEithers.bind(adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))(((htype: HaskellSyntax.Type) => LibEithers.bind(Annotations.getTypeDescription(cx)(g)(ftype))(((comments: string | null) => ({ tag: "right", value: ({
    field: ({
    name: hname_,
    type: htype
  }),
    comments: comments
  }) })))));
})());
  return LibEithers.bind(LibEithers.mapList(toField)(fields))(((hFields: ReadonlyArray<HaskellSyntax.FieldWithComments>) => ({ tag: "right", value: ({
    body: ({ tag: "record", value: ({
    name: HaskellUtils.simpleName(lname_),
    fields: hFields
  }) }),
    comments: null
  }) })));
})()));
  const unionCons = ((boundNames_: ReadonlySet<Core.Name>) => ((lname_: string) => ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  const ftype = ((_x) => _x.type)(fieldType);
  const deconflict = ((name: string) => (() => {
  const tname = Names.unqualifyName(({
    namespace: LibPairs.first(((_x) => _x.focus)(namespaces)),
    local: name
  }));
  return LibLogic.ifElse(LibSets.member(tname)(boundNames_))(deconflict(LibStrings.cat2(name)("_")))(name);
})());
  return LibEithers.bind(Annotations.getTypeDescription(cx)(g)(ftype))(((comments: string | null) => (() => {
  const nm = deconflict(LibStrings.cat2(Formatting.capitalize(lname_))(Formatting.capitalize(((_x) => _x)(fname))));
  return LibEithers.bind(LibLogic.ifElse(LibEquality.equal(Strip.deannotateType(ftype))(({ tag: "unit" })))(({ tag: "right", value: [] }))(LibEithers.bind(adaptTypeToHaskellAndEncode(namespaces)(ftype)(cx)(g))(((htype: HaskellSyntax.Type) => ({ tag: "right", value: [htype] })))))(((typeList: ReadonlyArray<HaskellSyntax.Type>) => ({ tag: "right", value: ({
    body: ({ tag: "ordinary", value: ({
    name: HaskellUtils.simpleName(nm),
    fields: typeList
  }) }),
    comments: comments
  }) })));
})()));
})())));
  return LibEithers.bind(Predicates.isSerializableByName(cx)(g)(elementName))(((isSer: boolean) => (() => {
  const deriv = LibLogic.ifElse(isSer)(LibLists.map(HaskellUtils.rawName)(["Eq", "Ord", "Read", "Show"]))([]);
  const unpackResult = HaskellUtils.unpackForallType(typ);
  const vars = LibPairs.first(unpackResult);
  const t_ = LibPairs.second(unpackResult);
  const hd = declHead(hname)(LibLists.reverse(vars));
  return LibEithers.bind((() => {
  const _m = Strip.deannotateType(t_);
  switch (_m.tag) {
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEithers.bind(recordCons(lname)(rt))(((cons: HaskellSyntax.ConstructorWithComments) => ({ tag: "right", value: ({ tag: "data", value: ({
    keyword: ({ tag: "data" }),
    context: [],
    head: hd,
    constructors: [cons],
    deriving: [deriv]
  }) }) }))))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapList(((v1: Core.FieldType) => unionCons(LibSets.fromList(LibMaps.keys(((_x) => _x.boundTerms)(g))))(lname)(v1)))(rt))(((cons: ReadonlyArray<HaskellSyntax.ConstructorWithComments>) => ({ tag: "right", value: ({ tag: "data", value: ({
    keyword: ({ tag: "data" }),
    context: [],
    head: hd,
    constructors: cons,
    deriving: [deriv]
  }) }) }))))((_m as any).value);
    case "wrap": return ((wrapped: Core.Type) => LibEithers.bind(newtypeCons(elementName)(wrapped))(((cons: HaskellSyntax.ConstructorWithComments) => ({ tag: "right", value: ({ tag: "data", value: ({
    keyword: ({ tag: "newtype" }),
    context: [],
    head: hd,
    constructors: [cons],
    deriving: [deriv]
  }) }) }))))((_m as any).value);
    default: return LibEithers.bind(adaptTypeToHaskellAndEncode(namespaces)(typ)(cx)(g))(((htype: HaskellSyntax.Type) => ({ tag: "right", value: ({ tag: "type", value: ({
    name: hd,
    type: htype
  }) }) })))(_m);
  }
})())(((decl: HaskellSyntax.Declaration) => LibEithers.bind(Annotations.getTypeDescription(cx)(g)(typ))(((comments: string | null) => LibEithers.bind(LibLogic.ifElse(includeTypeDefinitions)(LibEithers.bind(typeDecl(namespaces)(elementName)(typ)(cx)(g))(((decl_: HaskellSyntax.DeclarationWithComments) => ({ tag: "right", value: [decl_] }))))(({ tag: "right", value: [] })))(((tdecls: ReadonlyArray<HaskellSyntax.DeclarationWithComments>) => (() => {
  const mainDecl = ({
    body: decl,
    comments: comments
  });
  const nameDecls_ = nameDecls(namespaces)(elementName)(typ);
  return ({ tag: "right", value: LibLists.concat([[mainDecl], nameDecls_, tdecls]) });
})()))))));
})()));
})()))));
}

export function typeDecl<t0>(namespaces: Packaging.Namespaces<HaskellSyntax.ModuleName>): ((x: Core.Name) => ((x: Core.Type) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | HaskellSyntax.DeclarationWithComments)))) {
  return ((name: Core.Name) => ((typ: Core.Type) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const typeName = ((ns: Packaging.Namespace) => ((name_: Core.Name) => Names.qname(ns)(typeNameLocal(name_))));
  const typeNameLocal = ((name_: Core.Name) => LibStrings.cat(["_", Names.localNameOf(name_), "_type_"]));
  const rawTerm = EncodeCore.type(typ);
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const variantResult = (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(inj))("hydra.core.Type"))(((_x) => _x.field)(inj))(null))((_m as any).value);
    default: return null(_m);
  }
})();
  const decodeString = ((term2: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(term2);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "string": return ((s: string) => s)((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})());
  const decodeName = ((term2: Core.Term) => (() => {
  const _m = Strip.deannotateTerm(term2);
  switch (_m.tag) {
    case "wrap": return ((wt: Core.WrappedTerm) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(wt))("hydra.core.Name"))(LibMaybes.map(((x: string) => x))(decodeString(((_x) => _x.body)(wt))))(null))((_m as any).value);
    default: return null(_m);
  }
})());
  const forType = ((field: Core.Field) => (() => {
  const fname = ((_x) => _x.name)(field);
  const fterm = ((_x) => _x.term)(field);
  return LibLogic.ifElse(LibEquality.equal(fname)("record"))(null)(LibLogic.ifElse(LibEquality.equal(fname)("variable"))(LibMaybes.bind(decodeName(fterm))(forVariableType))(null));
})());
  const forVariableType = ((vname: Core.Name) => (() => {
  const qname = Names.qualifyName(vname);
  const mns = ((_x) => _x.namespace)(qname);
  const local = ((_x) => _x.local)(qname);
  return LibMaybes.map(((ns: Packaging.Namespace) => ({ tag: "variable", value: Names.qname(ns)(LibStrings.cat(["_", local, "_type_"])) })))(mns);
})());
  return LibMaybes.fromMaybe(recurse(term))(LibMaybes.bind(variantResult)(forType));
})()));
  const finalTerm = Rewriting.rewriteTerm(rewrite)(rawTerm);
  return LibEithers.bind(encodeTerm(0)(namespaces)(finalTerm)(cx)(g))(((expr: HaskellSyntax.Expression) => (() => {
  const rhs = expr;
  const hname = HaskellUtils.simpleName(typeNameLocal(name));
  const pat = HaskellUtils.applicationPattern(hname)([]);
  const decl = ({ tag: "valueBinding", value: ({ tag: "simple", value: ({
    pattern: pat,
    rhs: rhs,
    localBindings: null
  }) }) });
  return ({ tag: "right", value: ({
    body: decl,
    comments: null
  }) });
})()));
})()))));
}

export function typeSchemeConstraintsToClassMap<t0>(maybeConstraints: ReadonlyMap<t0, Core.TypeVariableMetadata> | null): ReadonlyMap<t0, ReadonlySet<Classes.TypeClass>> {
  return (() => {
  const nameToTypeClass = ((className: Core.Name) => (() => {
  const classNameStr = ((_x) => _x)(className);
  const isEq = LibEquality.equal(classNameStr)(((_x) => _x)("equality"));
  const isOrd = LibEquality.equal(classNameStr)(((_x) => _x)("ordering"));
  return LibLogic.ifElse(isEq)(({ tag: "equality" }))(LibLogic.ifElse(isOrd)(({ tag: "ordering" }))(null));
})());
  return LibMaybes.maybe(LibMaps.empty)(((constraints: ReadonlyMap<t0, Core.TypeVariableMetadata>) => LibMaps.map(((meta: Core.TypeVariableMetadata) => LibSets.fromList(LibMaybes.cat(LibLists.map(nameToTypeClass)(LibSets.toList(((_x) => _x.classes)(meta)))))))(constraints)))(maybeConstraints);
})();
}

export const useCoreImport: boolean = true;
