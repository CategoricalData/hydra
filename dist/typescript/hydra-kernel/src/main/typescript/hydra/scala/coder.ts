// Note: this is an automatically generated file. Do not edit.

/**
 * Scala code generator: converts Hydra modules to Scala source code
 */



import * as Analysis from "../analysis.js";
import * as Annotations from "../annotations.js";
import * as Arity from "../arity.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Environment from "../environment.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as Inference from "../inference.js";
import * as JsonModel from "../json/model.js";
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
import * as Reduction from "../reduction.js";
import * as Relational from "../relational.js";
import * as Resolution from "../resolution.js";
import * as ScalaLanguage from "./language.js";
import * as ScalaSerde from "./serde.js";
import * as ScalaSyntax from "./syntax.js";
import * as ScalaUtils from "./utils.js";
import * as Scoping from "../scoping.js";
import * as Serialization from "../serialization.js";
import * as ShowCore from "../show/core.js";
import * as Sorting from "../sorting.js";
import * as Strip from "../strip.js";
import * as Tabular from "../tabular.js";
import * as Testing from "../testing.js";
import * as Topology from "../topology.js";
import * as Typing from "../typing.js";
import * as Util from "../util.js";
import * as Variables from "../variables.js";
import * as Variants from "../variants.js";

export function applyVar(fterm: Core.Term): ((x: Core.Name) => Core.Term) {
  return ((avar: Core.Name) => (() => {
  const v = ((_x) => _x)(avar);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(fterm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const lamParam = ((_x) => _x.parameter)(lam);
  const lamBody = ((_x) => _x.body)(lam);
  return LibLogic.ifElse(Variables.isFreeVariableInTerm(lamParam)(lamBody))(lamBody)(Variables.substituteVariable(lamParam)(avar)(lamBody));
})())((_m as any).value);
    default: return ({ tag: "application", value: ({
    function: fterm,
    argument: ({ tag: "variable", value: avar })
  }) })(_m);
  }
})();
})());
}

export function constructModule<t0>(cx: t0): ((x: Graph.Graph) => ((x: Packaging.Module) => ((x: ReadonlyArray<Packaging.Definition>) => Errors.Error | ScalaSyntax.Pkg))) {
  return ((g: Graph.Graph) => ((mod: Packaging.Module) => ((defs: ReadonlyArray<Packaging.Definition>) => (() => {
  const partitioned = Environment.partitionDefinitions(defs);
  const typeDefs = LibPairs.first(partitioned);
  const termDefs = LibPairs.second(partitioned);
  const nsName = ((_x) => _x)(((_x) => _x.namespace)(mod));
  const pname = ({
    value: LibStrings.intercalate(".")(LibStrings.splitOn(".")(nsName))
  });
  const pref = ({ tag: "name", value: pname });
  return LibEithers.bind(LibEithers.mapList(((td: Packaging.TypeDefinition) => encodeTypeDefinition(cx)(g)(td)))(typeDefs))(((typeDeclStats: ReadonlyArray<ScalaSyntax.Stat>) => LibEithers.bind(LibEithers.mapList(((td: Packaging.TermDefinition) => encodeTermDefinition(cx)(g)(td)))(termDefs))(((termDeclStats: ReadonlyArray<ScalaSyntax.Stat>) => LibEithers.bind(findImports(cx)(g)(mod))(((imports: ReadonlyArray<ScalaSyntax.Stat>) => ({ tag: "right", value: ({
    name: pname,
    ref: pref,
    stats: LibLists.concat([imports, typeDeclStats, termDeclStats])
  }) })))))));
})())));
}

export function dropDomains(n: number): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => LibLogic.ifElse(LibEquality.lte(n)(0))(t)((() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => dropDomains(LibMath.sub(n)(1))(((_x) => _x.codomain)(ft)))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => dropDomains(n)(((_x) => _x.body)(fa)))((_m as any).value);
    default: return t(_m);
  }
})()));
}

export function encodeCase<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Name, Core.Type>) => ((x: Core.Name | null) => ((x: Core.Field) => Errors.Error | ScalaSyntax.Case)))) {
  return ((g: Graph.Graph) => ((ftypes: ReadonlyMap<Core.Name, Core.Type>) => ((sn: Core.Name | null) => ((f: Core.Field) => (() => {
  const fname = ((_x) => _x.name)(f);
  const fterm = ((_x) => _x.term)(f);
  const isUnit = LibMaybes.maybe((() => {
  const _m = Strip.deannotateAndDetypeTerm(fterm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const lamParam = ((_x) => _x.parameter)(lam);
  const lamBody = ((_x) => _x.body)(lam);
  const domIsUnit = LibMaybes.maybe(false)(((dom: Core.Type) => LibEquality.equal(dom)(({ tag: "unit" }))))(((_x) => _x.domain)(lam));
  const bodyIgnoresParam = Variables.isFreeVariableInTerm(lamParam)(lamBody);
  return LibLogic.or(domIsUnit)(bodyIgnoresParam);
})())((_m as any).value);
    case "record": return ((r: Core.Record) => LibEquality.equal(LibLists.length(((_x) => _x.fields)(r)))(0))((_m as any).value);
    case "unit": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})())(((dom: Core.Type) => (() => {
  const _m = Strip.deannotateType(dom);
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEquality.equal(LibLists.length(rt))(0))((_m as any).value);
    default: return false(_m);
  }
})()))(LibMaps.lookup(fname)(ftypes));
  const shortTypeName = LibLists.last(LibStrings.splitOn(".")(LibMaybes.maybe("x")(((n: Core.Name) => ((_x) => _x)(n)))(sn)));
  const lamParamSuffix = (() => {
  const _m = Strip.deannotateAndDetypeTerm(fterm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const rawName = ((_x) => _x)(((_x) => _x.parameter)(lam));
  const safeName = LibStrings.fromList(LibLists.map(((c: number) => LibLogic.ifElse(LibEquality.equal(c)(39))(95)(c)))(LibStrings.toList(rawName)));
  return LibStrings.cat2("_")(safeName);
})())((_m as any).value);
    default: return ""(_m);
  }
})();
  const v = LibStrings.cat(["v_", shortTypeName, "_", ((_x) => _x)(fname), lamParamSuffix]);
  const domainIsUnit = (() => {
  const _m = Strip.deannotateAndDetypeTerm(fterm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibMaybes.maybe(true)(((dom: Core.Type) => LibEquality.equal(dom)(({ tag: "unit" }))))(((_x) => _x.domain)(lam)))((_m as any).value);
    default: return true(_m);
  }
})();
  const patArgs = LibLogic.ifElse(isUnit)(LibLogic.ifElse(domainIsUnit)([])([({ tag: "wildcard" })]))([ScalaUtils.svar(v)]);
  const pat = ({ tag: "extract", value: ({
    fun: ScalaUtils.sname(ScalaUtils.qualifyUnionFieldName("MATCHED.")(sn)(fname)),
    args: patArgs
  }) });
  const applied = applyVar(fterm)(v);
  return LibEithers.bind(encodeTerm(cx)(g)(applied))(((body: ScalaSyntax.Data) => ({ tag: "right", value: ({
    pat: pat,
    cond: null,
    body: body
  }) })));
})()))));
}

export function encodeComplexTermDef<t0>(cx: t0): ((x: Graph.Graph) => ((x: string) => ((x: Core.Term) => ((x: Core.Type) => Errors.Error | ScalaSyntax.Stat)))) {
  return ((g: Graph.Graph) => ((lname: string) => ((term: Core.Term) => ((typ: Core.Type) => (() => {
  const doms = extractDomains(typ);
  const paramNames = extractParams(term);
  const paramCount = LibMath.min(LibLists.length(paramNames))(LibLists.length(doms));
  const cod = dropDomains(paramCount)(typ);
  const zippedParams = LibLists.zip(LibLists.take(paramCount)(paramNames))(LibLists.take(paramCount)(doms));
  const freeTypeVars = LibLists.filter(((v: Core.Name) => LibLogic.not(LibLists.elem(46)(LibStrings.toList(((_x) => _x)(v))))))(LibSets.toList(Variables.freeVariablesInType(typ)));
  const tparams = LibLists.map(((tv: Core.Name) => ScalaUtils.stparam(tv)))(freeTypeVars);
  const letBindings = extractLetBindings(term);
  const gWithTypeVars = ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: LibSets.union(LibSets.fromList(freeTypeVars))(((_x) => _x.typeVariables)(g))
  });
  return LibEithers.bind(LibEithers.mapList(((v1: readonly [Core.Name, Core.Type]) => encodeTypedParam(cx)(gWithTypeVars)(v1)))(zippedParams))(((sparams: ReadonlyArray<ScalaSyntax.Data_Param>) => LibEithers.bind(encodeTerm(cx)(gWithTypeVars)(extractBody(term)))(((sbody: ScalaSyntax.Data) => LibEithers.bind(encodeType(cx)(g)(cod))(((scod: ScalaSyntax.Type) => (() => {
  const gForLets = LibLogic.ifElse(LibLists.null_(letBindings))(gWithTypeVars)(Scoping.extendGraphForLet(((g2: Graph.Graph) => ((b: Core.Binding) => LibLogic.ifElse(Predicates.isComplexBinding(g2)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))))(gWithTypeVars)(({
    bindings: letBindings,
    body: ({ tag: "variable", value: "dummy" })
  })));
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeLetBinding(cx)(gForLets)(LibSets.fromList(freeTypeVars))(v1)))(letBindings))(((sbindings: ReadonlyArray<ScalaSyntax.Stat>) => (() => {
  const defBody = LibLogic.ifElse(LibLists.null_(sbindings))(sbody)(({ tag: "block", value: ({
    stats: LibLists.concat2(sbindings)([({ tag: "term", value: sbody })])
  }) }));
  return ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "def", value: ({
    mods: [],
    name: ({
    value: lname
  }),
    tparams: tparams,
    paramss: LibLists.map(((p: ScalaSyntax.Data_Param) => [p]))(sparams),
    decltpe: scod,
    body: defBody
  }) }) }) });
})()));
})()))))));
})()))));
}

export function encodeFunction<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Name, Core.Term>) => ((x: Core.Term) => ((x: Core.Term | null) => Errors.Error | ScalaSyntax.Data)))) {
  return ((g: Graph.Graph) => ((meta: ReadonlyMap<Core.Name, Core.Term>) => ((funTerm: Core.Term) => ((arg: Core.Term | null) => (() => {
  const _m = Strip.deannotateAndDetypeTerm(funTerm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const param = ((_x) => _x.parameter)(lam);
  const v = ScalaUtils.scalaEscapeName(((_x) => _x)(param));
  const body = ((_x) => _x.body)(lam);
  const rawMdom = ((_x) => _x.domain)(lam);
  const mdom = LibMaybes.bind(rawMdom)(((dom: Core.Type) => (() => {
  const freeVars = Variables.freeVariablesInType(dom);
  const unqualifiedFreeVars = LibSets.fromList(LibLists.filter(((n: Core.Name) => LibLogic.not(LibLists.elem(46)(LibStrings.toList(((_x) => _x)(n))))))(LibSets.toList(freeVars)));
  const unresolvedVars = LibSets.difference(unqualifiedFreeVars)(((_x) => _x.typeVariables)(g));
  return LibLogic.ifElse(LibSets.null_(unresolvedVars))(dom)(null);
})()));
  return LibEithers.bind(encodeTerm(cx)(g)(body))(((sbody: ScalaSyntax.Data) => LibEithers.bind(LibMaybes.maybe(findSdom(cx)(g)(meta))(((dom: Core.Type) => LibEithers.bind(encodeType(cx)(g)(dom))(((sdom: ScalaSyntax.Type) => ({ tag: "right", value: sdom })))))(mdom))(((sdom: ScalaSyntax.Type | null) => ({ tag: "right", value: ScalaUtils.slambda(v)(sbody)(sdom) })))));
})())((_m as any).value);
    case "unwrap": return ((name: Core.Name) => LibMaybes.maybe(LibEithers.bind(findSdom(cx)(g)(meta))(((sdom: ScalaSyntax.Type | null) => ({ tag: "right", value: ScalaUtils.slambda("x")(ScalaUtils.sname("x"))(sdom) }))))(((a: Core.Term) => encodeTerm(cx)(g)(a)))(arg))((_m as any).value);
    case "project": return ((proj: Core.Projection) => (() => {
  const fname = ScalaUtils.scalaEscapeName(((_x) => _x)(((_x) => _x.field)(proj)));
  const typeName = ((_x) => _x.typeName)(proj);
  const pv = "x";
  return LibMaybes.maybe(LibEithers.bind(LibEithers.either(((_: Errors.Error) => LibEithers.bind(encodeType(cx)(g)(({ tag: "variable", value: typeName })))(((st: ScalaSyntax.Type) => ({ tag: "right", value: st })))))(((msdom: ScalaSyntax.Type | null) => LibMaybes.maybe(LibEithers.bind(encodeType(cx)(g)(({ tag: "variable", value: typeName })))(((st: ScalaSyntax.Type) => ({ tag: "right", value: st }))))(((sdom: ScalaSyntax.Type) => ({ tag: "right", value: sdom })))(msdom)))(findSdom(cx)(g)(meta)))(((msdom: ScalaSyntax.Type | null) => ({ tag: "right", value: ScalaUtils.slambda(pv)(({ tag: "ref", value: ({ tag: "select", value: ({
    qual: ScalaUtils.sname(pv),
    name: ({
    value: fname
  })
  }) }) }))(msdom) }))))(((a: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(a))(((sa: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "select", value: ({
    qual: sa,
    name: ({
    value: fname
  })
  }) }) }) })))))(arg);
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const v = "v";
  const tname = ((_x) => _x.typeName)(cs);
  const dom = ({ tag: "variable", value: tname });
  const sn = ScalaUtils.nameOfType(g)(dom);
  const cases = ((_x) => _x.cases)(cs);
  const dflt = ((_x) => _x.default)(cs);
  const ftypes = LibEithers.either(((_: Errors.Error) => LibMaps.empty))(((x_: ReadonlyMap<Core.Name, Core.Type>) => x_))(Resolution.fieldTypes(cx)(g)(dom));
  return LibEithers.bind(LibEithers.mapList(((f: Core.Field) => encodeCase(cx)(g)(ftypes)(sn)(f)))(cases))(((fieldCases: ReadonlyArray<ScalaSyntax.Case>) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: fieldCases }))(((dfltTerm: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(dfltTerm))(((sdflt: ScalaSyntax.Data) => ({ tag: "right", value: LibLists.concat2(fieldCases)([({
    pat: ({ tag: "wildcard" }),
    cond: null,
    body: sdflt
  })]) })))))(dflt))(((scases: ReadonlyArray<ScalaSyntax.Case>) => LibMaybes.maybe(LibEithers.bind(findSdom(cx)(g)(meta))(((sdom: ScalaSyntax.Type | null) => ({ tag: "right", value: ScalaUtils.slambda(v)(({ tag: "match", value: ({
    expr: ScalaUtils.sname(v),
    cases: scases
  }) }))(sdom) }))))(((a: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(a))(((sa: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "match", value: ({
    expr: sa,
    cases: scases
  }) }) })))))(arg)))));
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported function" }) })(_m);
  }
})()))));
}

export function encodeLetBinding<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlySet<Core.Name>) => ((x: Core.Binding) => Errors.Error | ScalaSyntax.Stat))) {
  return ((g: Graph.Graph) => ((outerTypeVars: ReadonlySet<Core.Name>) => ((b: Core.Binding) => (() => {
  const bname = ScalaUtils.scalaEscapeName(((_x) => _x)(((_x) => _x.name)(b)));
  const bterm = ((_x) => _x.term)(b);
  const mts = LibMaybes.maybe(LibMaps.lookup(((_x) => _x.name)(b))(((_x) => _x.boundTypes)(g)))(((ts: Core.TypeScheme) => ts))(((_x) => _x.type)(b));
  const isFn = LibMaybes.maybe(false)(((ts: Core.TypeScheme) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.type)(ts));
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => true)((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.body)(fa));
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})()))(mts);
  return LibMaybes.maybe(LibEithers.bind(encodeTerm(cx)(g)(bterm))(((srhs: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "val", value: ({
    mods: [({ tag: "lazy" })],
    pats: [({ tag: "var", value: ({
    name: ({
    value: bname
  })
  }) })],
    decltpe: null,
    rhs: srhs
  }) }) }) }))))(((ts: Core.TypeScheme) => (() => {
  const newVars = LibLists.filter(((v: Core.Name) => LibLogic.not(LibSets.member(v)(outerTypeVars))))(((_x) => _x.variables)(ts));
  const useDef = LibLogic.or(isFn)(LibLogic.not(LibLists.null_(newVars)));
  return LibLogic.ifElse(useDef)(encodeLocalDef(cx)(g)(outerTypeVars)(bname)(bterm)(((_x) => _x.type)(ts)))(LibEithers.bind(encodeTerm(cx)(g)(bterm))(((srhs: ScalaSyntax.Data) => LibEithers.bind(encodeType(cx)(g)(((_x) => _x.type)(ts)))(((styp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "val", value: ({
    mods: [({ tag: "lazy" })],
    pats: [({ tag: "var", value: ({
    name: ({
    value: bname
  })
  }) })],
    decltpe: styp,
    rhs: srhs
  }) }) }) }))))));
})()))(mts);
})())));
}

export function encodeLiteral<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Literal) => Errors.Error | ScalaSyntax.Lit)) {
  return ((g: t1) => ((av: Core.Literal) => (() => {
  const _m = av;
  switch (_m.tag) {
    case "binary": return ((b: Uint8Array) => ({ tag: "right", value: ({ tag: "bytes", value: LibLiterals.binaryToBytes(b) }) }))((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "right", value: ({ tag: "boolean", value: b }) }))((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((bf: number) => ({ tag: "right", value: ({ tag: "double", value: LibLiterals.bigfloatToFloat64(bf) }) }))((_m as any).value);
    case "float32": return ((f: number) => ({ tag: "right", value: ({ tag: "float", value: f }) }))((_m as any).value);
    case "float64": return ((f: number) => ({ tag: "right", value: ({ tag: "double", value: f }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unexpected float value" }) })(_m);
  }
})())((_m as any).value);
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => ({ tag: "right", value: ({ tag: "long", value: LibLiterals.bigintToInt64(i) }) }))((_m as any).value);
    case "int8": return ((i: number) => ({ tag: "right", value: ({ tag: "byte", value: i }) }))((_m as any).value);
    case "int16": return ((i: bigint) => ({ tag: "right", value: ({ tag: "short", value: i }) }))((_m as any).value);
    case "int32": return ((i: number) => ({ tag: "right", value: ({ tag: "int", value: i }) }))((_m as any).value);
    case "int64": return ((i: bigint) => ({ tag: "right", value: ({ tag: "long", value: i }) }))((_m as any).value);
    case "uint8": return ((i: bigint) => ({ tag: "right", value: ({ tag: "byte", value: LibLiterals.bigintToInt8(LibLiterals.uint8ToBigint(i)) }) }))((_m as any).value);
    case "uint16": return ((i: number) => ({ tag: "right", value: ({ tag: "int", value: LibLiterals.bigintToInt32(LibLiterals.uint16ToBigint(i)) }) }))((_m as any).value);
    case "uint32": return ((i: bigint) => ({ tag: "right", value: ({ tag: "long", value: LibLiterals.bigintToInt64(LibLiterals.uint32ToBigint(i)) }) }))((_m as any).value);
    case "uint64": return ((i: bigint) => ({ tag: "right", value: ({ tag: "long", value: LibLiterals.bigintToInt64(LibLiterals.uint64ToBigint(i)) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unexpected integer value" }) })(_m);
  }
})())((_m as any).value);
    case "string": return ((s: string) => ({ tag: "right", value: ({ tag: "string", value: s }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unexpected literal" }) })(_m);
  }
})()));
}

export function encodeLocalDef<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlySet<Core.Name>) => ((x: string) => ((x: Core.Term) => ((x: Core.Type) => Errors.Error | ScalaSyntax.Stat))))) {
  return ((g: Graph.Graph) => ((outerTypeVars: ReadonlySet<Core.Name>) => ((lname: string) => ((term: Core.Term) => ((typ: Core.Type) => (() => {
  const freeTypeVars = LibLists.filter(((v: Core.Name) => LibLogic.and(LibLogic.not(LibLists.elem(46)(LibStrings.toList(((_x) => _x)(v)))))(LibLogic.not(LibSets.member(v)(outerTypeVars)))))(LibSets.toList(Variables.freeVariablesInType(typ)));
  const doms = extractDomains(typ);
  const paramNames = extractParams(term);
  const paramCount = LibMath.min(LibLists.length(paramNames))(LibLists.length(doms));
  const cod = dropDomains(paramCount)(typ);
  const zippedParams = LibLists.zip(LibLists.take(paramCount)(paramNames))(LibLists.take(paramCount)(doms));
  const letBindings = extractLetBindings(term);
  const tparams = LibLists.map(((tv: Core.Name) => ScalaUtils.stparam(tv)))(freeTypeVars);
  const allTypeVars = LibSets.union(outerTypeVars)(LibSets.fromList(freeTypeVars));
  const gWithTypeVars = ({
    boundTerms: ((_x) => _x.boundTerms)(g),
    boundTypes: ((_x) => _x.boundTypes)(g),
    classConstraints: ((_x) => _x.classConstraints)(g),
    lambdaVariables: ((_x) => _x.lambdaVariables)(g),
    metadata: ((_x) => _x.metadata)(g),
    primitives: ((_x) => _x.primitives)(g),
    schemaTypes: ((_x) => _x.schemaTypes)(g),
    typeVariables: LibSets.union(allTypeVars)(((_x) => _x.typeVariables)(g))
  });
  return LibEithers.bind(LibEithers.mapList(((v1: readonly [Core.Name, Core.Type]) => encodeTypedParam(cx)(gWithTypeVars)(v1)))(zippedParams))(((sparams: ReadonlyArray<ScalaSyntax.Data_Param>) => LibEithers.bind(encodeTerm(cx)(gWithTypeVars)(extractBody(term)))(((sbody: ScalaSyntax.Data) => LibEithers.bind(encodeType(cx)(gWithTypeVars)(cod))(((scod: ScalaSyntax.Type) => (() => {
  const gForLets = LibLogic.ifElse(LibLists.null_(letBindings))(gWithTypeVars)(Scoping.extendGraphForLet(((g2: Graph.Graph) => ((b: Core.Binding) => LibLogic.ifElse(Predicates.isComplexBinding(g2)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))))(gWithTypeVars)(({
    bindings: letBindings,
    body: ({ tag: "variable", value: "dummy" })
  })));
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeLetBinding(cx)(gForLets)(allTypeVars)(v1)))(letBindings))(((sbindings: ReadonlyArray<ScalaSyntax.Stat>) => (() => {
  const defBody = LibLogic.ifElse(LibLists.null_(sbindings))(sbody)(({ tag: "block", value: ({
    stats: LibLists.concat2(sbindings)([({ tag: "term", value: sbody })])
  }) }));
  return ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "def", value: ({
    mods: [],
    name: ({
    value: lname
  }),
    tparams: tparams,
    paramss: LibLists.map(((p: ScalaSyntax.Data_Param) => [p]))(sparams),
    decltpe: scod,
    body: defBody
  }) }) }) });
})()));
})()))))));
})())))));
}

export function encodeTerm<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ScalaSyntax.Data)) {
  return ((g: Graph.Graph) => ((term0: Core.Term) => (() => {
  const term = stripWrapEliminations(term0);
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const collectTypeArgs = ((t: Core.Term) => ((acc: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "typeApplication": return ((ta2: Core.TypeApplicationTerm) => collectTypeArgs(((_x) => _x.body)(ta2))(LibLists.cons(((_x) => _x.type)(ta2))(acc)))((_m as any).value);
    default: return [acc, t](_m);
  }
})()));
  const collected = collectTypeArgs(((_x) => _x.body)(ta))([((_x) => _x.type)(ta)]);
  const typeArgs = LibPairs.first(collected);
  const innerTerm = LibPairs.second(collected);
  const collectTypeLambdas = ((t: Core.Term) => ((acc: ReadonlyArray<Core.Name>) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "typeLambda": return ((tl: Core.TypeLambda) => collectTypeLambdas(((_x) => _x.body)(tl))(LibLists.cons(((_x) => _x.parameter)(tl))(acc)))((_m as any).value);
    default: return [acc, t](_m);
  }
})()));
  const tlCollected = collectTypeLambdas(innerTerm)([]);
  const typeParams = LibPairs.first(tlCollected);
  const bodyAfterTypeLambdas = LibPairs.second(tlCollected);
  const substitutedBody = bodyAfterTypeLambdas;
  return (() => {
  const _m = Strip.deannotateTerm(substitutedBody);
  switch (_m.tag) {
    case "project": return ((_: Core.Projection) => encodeTerm(cx)(g)(substitutedBody))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => encodeTerm(cx)(g)(substitutedBody))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => encodeTerm(cx)(g)(substitutedBody))((_m as any).value);
    case "variable": return ((pname: Core.Name) => LibMaybes.cases(LibMaps.lookup(pname)(((_x) => _x.primitives)(g)))(encodeTerm(cx)(g)(substitutedBody))(((_prim: Graph.Primitive) => LibEithers.bind(LibEithers.mapList(((targ: Core.Type) => encodeType(cx)(g)(targ)))(typeArgs))(((stypeArgs: ReadonlyArray<ScalaSyntax.Type>) => (() => {
  const inScopeTypeVarNames = LibSets.fromList(LibLists.map(((n: Core.Name) => Formatting.capitalize(((_x) => _x)(n))))(LibSets.toList(((_x) => _x.typeVariables)(g))));
  const hasForallResidual = LibLogic.not(LibLists.null_(LibLists.filter(((st: ScalaSyntax.Type) => (() => {
  const _m = st;
  switch (_m.tag) {
    case "var": return ((tv: ScalaSyntax.Type_Var) => (() => {
  const tvName = ((_x) => _x.value)(((_x) => _x.name)(tv));
  return LibLogic.and(LibLogic.not(LibLists.elem(46)(LibStrings.toList(tvName))))(LibLogic.not(LibSets.member(tvName)(inScopeTypeVarNames)));
})())((_m as any).value);
    default: return false(_m);
  }
})()))(stypeArgs)));
  return LibLogic.ifElse(hasForallResidual)(({ tag: "right", value: ScalaUtils.sprim(pname) }))(({ tag: "right", value: ScalaUtils.sapplyTypes(ScalaUtils.sprim(pname))(stypeArgs) }));
})())))))((_m as any).value);
    default: return encodeTerm(cx)(g)(substitutedBody)(_m);
  }
})();
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => encodeTerm(cx)(Scoping.extendGraphForTypeLambda(g)(tl))(((_x) => _x.body)(tl)))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  const arg = ((_x) => _x.argument)(app);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(fun);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const lamBody = ((_x) => _x.body)(lam);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(lamBody);
  switch (_m.tag) {
    case "application": return ((innerApp: Core.Application) => (() => {
  const innerFun = ((_x) => _x.function)(innerApp);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(innerFun);
  switch (_m.tag) {
    case "cases": return ((_: Core.CaseStatement) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(innerFun))(innerFun)(arg))((_m as any).value);
    default: return LibEithers.bind(encodeTerm(cx)(g)(fun))(((sfun: ScalaSyntax.Data) => LibEithers.bind(encodeTerm(cx)(g)(arg))(((sarg: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(sfun)([sarg]) })))))(_m);
  }
})();
})())((_m as any).value);
    default: return LibEithers.bind(encodeTerm(cx)(g)(fun))(((sfun: ScalaSyntax.Data) => LibEithers.bind(encodeTerm(cx)(g)(arg))(((sarg: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(sfun)([sarg]) })))))(_m);
  }
})();
})())((_m as any).value);
    case "project": return ((proj: Core.Projection) => (() => {
  const fname = ScalaUtils.scalaEscapeName(((_x) => _x)(((_x) => _x.field)(proj)));
  return LibEithers.bind(encodeTerm(cx)(g)(arg))(((sarg: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "select", value: ({
    qual: sarg,
    name: ({
    value: fname
  })
  }) }) }) })));
})())((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(fun))(fun)(arg))((_m as any).value);
    default: return LibEithers.bind(encodeTerm(cx)(g)(fun))(((sfun: ScalaSyntax.Data) => LibEithers.bind(encodeTerm(cx)(g)(arg))(((sarg: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(sfun)([sarg]) })))))(_m);
  }
})();
})())((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(term))(term)(null))((_m as any).value);
    case "project": return ((_: Core.Projection) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(term))(term)(null))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(term))(term)(null))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => encodeFunction(cx)(g)(Annotations.termAnnotationInternal(term))(term)(null))((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(((e: Core.Term) => encodeTerm(cx)(g)(e)))(els))(((sels: ReadonlyArray<ScalaSyntax.Data>) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Seq"))(sels) }))))((_m as any).value);
    case "literal": return ((v: Core.Literal) => LibEithers.bind(encodeLiteral(cx)(g)(v))(((slit: ScalaSyntax.Lit) => (() => {
  const litData = ({ tag: "lit", value: slit });
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((bi: bigint) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("BigInt"))([({ tag: "lit", value: ({ tag: "string", value: LibLiterals.showBigint(bi) }) })]) }))((_m as any).value);
    case "uint64": return ((ui: bigint) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("BigInt"))([({ tag: "lit", value: ({ tag: "string", value: LibLiterals.showBigint(LibLiterals.uint64ToBigint(ui)) }) })]) }))((_m as any).value);
    default: return ({ tag: "right", value: litData })(_m);
  }
})())((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((_: number) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("BigDecimal"))([litData]) }))((_m as any).value);
    default: return ({ tag: "right", value: litData })(_m);
  }
})())((_m as any).value);
    default: return ({ tag: "right", value: litData })(_m);
  }
})();
})())))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.bind(LibEithers.mapList(((kv: readonly [Core.Term, Core.Term]) => LibEithers.bind(encodeTerm(cx)(g)(LibPairs.first(kv)))(((sk: ScalaSyntax.Data) => LibEithers.bind(encodeTerm(cx)(g)(LibPairs.second(kv)))(((sv: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sassign(sk)(sv) })))))))(LibMaps.toList(m)))(((spairs: ReadonlyArray<ScalaSyntax.Data>) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Map"))(spairs) }))))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => encodeTerm(cx)(g)(((_x) => _x.body)(wt)))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: ScalaUtils.sname("None") }))(((t: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(t))(((s: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Some"))([s]) })))))(m))((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const rname = ((_x) => _x.typeName)(rec);
  const fields = ((_x) => _x.fields)(rec);
  const n = ScalaUtils.scalaTypeName(true)(rname);
  return LibEithers.bind(LibEithers.mapList(((f: Core.Field) => encodeTerm(cx)(g)(((_x) => _x.term)(f))))(fields))(((args: ReadonlyArray<ScalaSyntax.Data>) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname(n))(args) })));
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.bind(LibEithers.mapList(((e: Core.Term) => encodeTerm(cx)(g)(e)))(LibSets.toList(s)))(((sels: ReadonlyArray<ScalaSyntax.Data>) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("scala.collection.immutable.Set"))(sels) }))))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const sn = ((_x) => _x.typeName)(inj);
  const fn = ((_x) => _x.name)(((_x) => _x.field)(inj));
  const ft = ((_x) => _x.term)(((_x) => _x.field)(inj));
  const lhs = ScalaUtils.sname(ScalaUtils.qualifyUnionFieldName("UNION.")(sn)(fn));
  const unionFtypes = LibEithers.either(((_: Errors.Error) => LibMaps.empty))(((x_: ReadonlyMap<Core.Name, Core.Type>) => x_))(Resolution.fieldTypes(cx)(g)(({ tag: "variable", value: sn })));
  return LibLogic.ifElse(LibMaybes.maybe((() => {
  const _m = Strip.deannotateAndDetypeTerm(ft);
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    case "record": return ((rec: Core.Record) => LibEquality.equal(LibLists.length(((_x) => _x.fields)(rec)))(0))((_m as any).value);
    default: return false(_m);
  }
})())(((dom: Core.Type) => (() => {
  const _m = Strip.deannotateType(dom);
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEquality.equal(LibLists.length(rt))(0))((_m as any).value);
    default: return false(_m);
  }
})()))(LibMaps.lookup(fn)(unionFtypes)))(({ tag: "right", value: lhs }))(LibEithers.bind(encodeTerm(cx)(g)(ft))(((sarg: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(lhs)([sarg]) }))));
})())((_m as any).value);
    case "variable": return ((v: Core.Name) => (() => {
  const fullName = ((_x) => _x)(v);
  const localName = Names.localNameOf(v);
  const parts = LibStrings.splitOn(".")(fullName);
  const numParts = LibLists.length(parts);
  const escaped = LibLogic.ifElse(LibEquality.lte(numParts)(1))(ScalaUtils.scalaEscapeName(fullName))(LibLogic.ifElse(LibEquality.equal(numParts)(2))(LibStrings.cat2(LibLists.head(parts))(LibStrings.cat2(".")(ScalaUtils.scalaEscapeName(localName))))(LibStrings.intercalate(".")(LibLists.concat2(LibLists.take(LibMath.sub(numParts)(1))(parts))([ScalaUtils.scalaEscapeName(localName)]))));
  return ({ tag: "right", value: ScalaUtils.sname(escaped) });
})())((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedTerm) => encodeTerm(cx)(g)(((_x) => _x.body)(at)))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(l))(((sl: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Left"))([sl]) })))))(((r: Core.Term) => LibEithers.bind(encodeTerm(cx)(g)(r))(((sr: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Right"))([sr]) })))))(e))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(encodeTerm(cx)(g)(LibPairs.first(p)))(((sf: ScalaSyntax.Data) => LibEithers.bind(encodeTerm(cx)(g)(LibPairs.second(p)))(((ss: ScalaSyntax.Data) => ({ tag: "right", value: ScalaUtils.sapply(ScalaUtils.sname("Tuple2"))([sf, ss]) }))))))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "lit", value: ({ tag: "unit" }) }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  const body = ((_x) => _x.body)(lt);
  const gLet = Scoping.extendGraphForLet(((g2: Graph.Graph) => ((b: Core.Binding) => LibLogic.ifElse(Predicates.isComplexBinding(g2)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))))(g)(lt);
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeLetBinding(cx)(gLet)(((_x) => _x.typeVariables)(gLet))(v1)))(bindings))(((sbindings: ReadonlyArray<ScalaSyntax.Stat>) => LibEithers.bind(encodeTerm(cx)(gLet)(body))(((sbody: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "block", value: ({
    stats: LibLists.concat2(sbindings)([({ tag: "term", value: sbody })])
  }) }) })))));
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unexpected term" }) })(_m);
  }
})();
})()));
}

export function encodeTermDefinition<t0>(cx: t0): ((x: Graph.Graph) => ((x: Packaging.TermDefinition) => Errors.Error | ScalaSyntax.Stat)) {
  return ((g: Graph.Graph) => ((td: Packaging.TermDefinition) => (() => {
  const name = ((_x) => _x.name)(td);
  const term = ((_x) => _x.term)(td);
  const lname = ScalaUtils.scalaEscapeName(Names.localNameOf(name));
  const typ_ = LibMaybes.maybe(({ tag: "variable", value: "hydra.core.Unit" }))(((_x) => _x.type))(((_x) => _x.type)(td));
  const isFunctionType = (() => {
  const _m = Strip.deannotateType(typ_);
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => true)((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.body)(fa));
  switch (_m.tag) {
    case "function": return ((_: Core.FunctionType) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})();
  return LibLogic.ifElse(isFunctionType)(encodeComplexTermDef(cx)(g)(lname)(term)(typ_))(LibEithers.bind(encodeType(cx)(g)(typ_))(((stype: ScalaSyntax.Type) => LibEithers.bind(encodeTerm(cx)(g)(term))(((rhs: ScalaSyntax.Data) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "val", value: ({
    mods: [({ tag: "lazy" })],
    pats: [({ tag: "var", value: ({
    name: ({
    value: lname
  })
  }) })],
    decltpe: stype,
    rhs: rhs
  }) }) }) }))))));
})()));
}

export function encodeType<t0, t1>(cx: t0): ((x: t1) => ((x: Core.Type) => Errors.Error | ScalaSyntax.Type)) {
  return ((g: t1) => ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "application": return ((at: Core.ApplicationType) => (() => {
  const collectTypeArgs = ((t2: Core.Type) => ((acc: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateType(t2);
  switch (_m.tag) {
    case "application": return ((at2: Core.ApplicationType) => (() => {
  const f2 = ((_x) => _x.function)(at2);
  const a2 = ((_x) => _x.argument)(at2);
  return collectTypeArgs(f2)(LibLists.cons(a2)(acc));
})())((_m as any).value);
    default: return [t2, acc](_m);
  }
})()));
  const collected = collectTypeArgs(({ tag: "application", value: at }))([]);
  const baseFun = LibPairs.first(collected);
  const allArgs = LibPairs.second(collected);
  return LibEithers.bind(encodeType(cx)(g)(baseFun))(((sfun: ScalaSyntax.Type) => LibEithers.bind(LibEithers.mapList(((a: Core.Type) => encodeType(cx)(g)(a)))(allArgs))(((sargs: ReadonlyArray<ScalaSyntax.Type>) => ({ tag: "right", value: ScalaUtils.stapply(sfun)(sargs) })))));
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Unit"
  }) }) }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => (() => {
  const lt = ((_x) => _x.left)(et);
  const rt = ((_x) => _x.right)(et);
  return LibEithers.bind(encodeType(cx)(g)(lt))(((slt: ScalaSyntax.Type) => LibEithers.bind(encodeType(cx)(g)(rt))(((srt: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply2(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Either"
  }) }) }))(slt)(srt) })))));
})())((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(ft);
  const cod = ((_x) => _x.codomain)(ft);
  return LibEithers.bind(encodeType(cx)(g)(dom))(((sdom: ScalaSyntax.Type) => LibEithers.bind(encodeType(cx)(g)(cod))(((scod: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "functionType", value: ({ tag: "function", value: ({
    params: [sdom],
    res: scod
  }) }) }) })))));
})())((_m as any).value);
    case "list": return ((lt: Core.Type) => LibEithers.bind(encodeType(cx)(g)(lt))(((slt: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply1(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Seq"
  }) }) }))(slt) }))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "right", value: ScalaUtils.stapply(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Array"
  }) }) }))([({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Byte"
  }) }) })]) }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Boolean"
  }) }) }) }))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "BigDecimal"
  }) }) }) }))((_m as any).value);
    case "float32": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Float"
  }) }) }) }))((_m as any).value);
    case "float64": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Double"
  }) }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported float type" }) })(_m);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "BigInt"
  }) }) }) }))((_m as any).value);
    case "int8": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Byte"
  }) }) }) }))((_m as any).value);
    case "int16": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Short"
  }) }) }) }))((_m as any).value);
    case "int32": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Int"
  }) }) }) }))((_m as any).value);
    case "int64": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Long"
  }) }) }) }))((_m as any).value);
    case "uint8": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Byte"
  }) }) }) }))((_m as any).value);
    case "uint16": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Int"
  }) }) }) }))((_m as any).value);
    case "uint32": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Long"
  }) }) }) }))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "BigInt"
  }) }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported integer type" }) })(_m);
  }
})())((_m as any).value);
    case "string": return ((_: void) => ({ tag: "right", value: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: "scala.Predef.String"
  }) }) }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported literal type" }) })(_m);
  }
})())((_m as any).value);
    case "map": return ((mt: Core.MapType) => (() => {
  const kt = ((_x) => _x.keys)(mt);
  const vt = ((_x) => _x.values)(mt);
  return LibEithers.bind(encodeType(cx)(g)(kt))(((skt: ScalaSyntax.Type) => LibEithers.bind(encodeType(cx)(g)(vt))(((svt: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply2(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Map"
  }) }) }))(skt)(svt) })))));
})())((_m as any).value);
    case "maybe": return ((ot: Core.Type) => LibEithers.bind(encodeType(cx)(g)(ot))(((sot: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply1(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Option"
  }) }) }))(sot) }))))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => (() => {
  const ft = ((_x) => _x.first)(pt);
  const st = ((_x) => _x.second)(pt);
  return LibEithers.bind(encodeType(cx)(g)(ft))(((sft: ScalaSyntax.Type) => LibEithers.bind(encodeType(cx)(g)(st))(((sst: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply2(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "Tuple2"
  }) }) }))(sft)(sst) })))));
})())((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous record type" }) }))((_m as any).value);
    case "set": return ((st: Core.Type) => LibEithers.bind(encodeType(cx)(g)(st))(((sst: ScalaSyntax.Type) => ({ tag: "right", value: ScalaUtils.stapply1(({ tag: "ref", value: ({ tag: "name", value: ({
    value: "scala.collection.immutable.Set"
  }) }) }))(sst) }))))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous union type" }) }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous wrap type" }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(ft);
  const body = ((_x) => _x.body)(ft);
  return LibEithers.bind(encodeType(cx)(g)(body))(((sbody: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "lambda", value: ({
    tparams: [ScalaUtils.stparam(v)],
    tpe: sbody
  }) }) })));
})())((_m as any).value);
    case "variable": return ((v: Core.Name) => (() => {
  const rawName = ((_x) => _x)(v);
  const typeName = LibLogic.ifElse(LibLists.elem(46)(LibStrings.toList(rawName)))(rawName)(Formatting.capitalize(rawName));
  return ({ tag: "right", value: ({ tag: "var", value: ({
    name: ({
    value: typeName
  })
  }) }) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "unsupported type" }) })(_m);
  }
})()));
}

export function encodeTypeDefinition<t0, t1>(cx: t0): ((x: t1) => ((x: Packaging.TypeDefinition) => Errors.Error | ScalaSyntax.Stat)) {
  return ((g: t1) => ((td: Packaging.TypeDefinition) => (() => {
  const name = ((_x) => _x.name)(td);
  const typ = ((_x) => _x.type)(((_x) => _x.type)(td));
  const lname = Names.localNameOf(name);
  const tname = ({
    value: lname
  });
  const dname = ({
    value: lname
  });
  const freeVars = LibLists.filter(((v: Core.Name) => LibLogic.not(LibLists.elem(46)(LibStrings.toList(((_x) => _x)(v))))))(LibSets.toList(Variables.freeVariablesInType(typ)));
  const tparams = LibLists.map(((__v: Core.Name) => (() => {
  const vn = Formatting.capitalize(((_x) => _x)(__v));
  return ({
    mods: [],
    name: ({ tag: "value", value: vn }),
    tparams: [],
    tbounds: [],
    vbounds: [],
    cbounds: []
  });
})()))(freeVars);
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const forallBody = ((_x) => _x.body)(ft);
  const forallParam = ((_x) => _x.parameter)(ft);
  const collectForallParams = ((t: Core.Type) => ((acc: ReadonlyArray<Core.Name>) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((ft2: Core.ForallType) => collectForallParams(((_x) => _x.body)(ft2))(LibLists.cons(((_x) => _x.parameter)(ft2))(acc)))((_m as any).value);
    default: return [acc, t](_m);
  }
})()));
  const collected = collectForallParams(forallBody)([forallParam]);
  const allForallParams = LibLists.reverse(LibPairs.first(collected));
  const innerBody = LibPairs.second(collected);
  const allTparams = LibLists.map(((__v: Core.Name) => (() => {
  const vn = Formatting.capitalize(((_x) => _x)(__v));
  return ({
    mods: [],
    name: ({ tag: "value", value: vn }),
    tparams: [],
    tbounds: [],
    vbounds: [],
    cbounds: []
  });
})()))(allForallParams);
  return (() => {
  const _m = Strip.deannotateType(innerBody);
  switch (_m.tag) {
    case "record": return ((rt2: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => fieldToParam(cx)(g)(f)))(rt2))(((params: ReadonlyArray<ScalaSyntax.Data_Param>) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "class", value: ({
    mods: [({ tag: "case" })],
    name: tname,
    tparams: allTparams,
    ctor: ({
    mods: [],
    name: ({ tag: "value", value: "" }),
    paramss: [params]
  }),
    template: ({
    early: [],
    inits: [],
    self: undefined,
    stats: []
  })
  }) }) }) }))))((_m as any).value);
    case "union": return ((rt2: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => fieldToEnumCase(cx)(g)(lname)(allTparams)(f)))(rt2))(((cases: ReadonlyArray<ScalaSyntax.Stat>) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "enum", value: ({
    mods: [],
    name: tname,
    tparams: allTparams,
    ctor: ({
    mods: [],
    name: ({ tag: "value", value: "" }),
    paramss: []
  }),
    template: ({
    early: [],
    inits: [],
    self: undefined,
    stats: cases
  })
  }) }) }) }))))((_m as any).value);
    case "wrap": return ((wt2: Core.Type) => LibEithers.bind(encodeType(cx)(g)(wt2))(((styp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "type", value: ({
    mods: [],
    name: tname,
    tparams: allTparams,
    body: styp
  }) }) }) }))))((_m as any).value);
    default: return (() => {
  const mkAlias = ((styp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "type", value: ({
    mods: [],
    name: ({
    value: lname
  }),
    tparams: allTparams,
    body: styp
  }) }) }) }));
  return LibEithers.either(((_: Errors.Error) => mkAlias(ScalaUtils.stref("Any"))))(mkAlias)(encodeType(cx)(g)(innerBody));
})()(_m);
  }
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => fieldToParam(cx)(g)(f)))(rt))(((params: ReadonlyArray<ScalaSyntax.Data_Param>) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "class", value: ({
    mods: [({ tag: "case" })],
    name: tname,
    tparams: tparams,
    ctor: ({
    mods: [],
    name: ({ tag: "value", value: "" }),
    paramss: [params]
  }),
    template: ({
    early: [],
    inits: [],
    self: undefined,
    stats: []
  })
  }) }) }) }))))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => fieldToEnumCase(cx)(g)(lname)(tparams)(f)))(rt))(((cases: ReadonlyArray<ScalaSyntax.Stat>) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "enum", value: ({
    mods: [],
    name: tname,
    tparams: tparams,
    ctor: ({
    mods: [],
    name: ({ tag: "value", value: "" }),
    paramss: []
  }),
    template: ({
    early: [],
    inits: [],
    self: undefined,
    stats: cases
  })
  }) }) }) }))))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => LibEithers.bind(encodeType(cx)(g)(wt))(((styp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "type", value: ({
    mods: [],
    name: tname,
    tparams: tparams,
    body: styp
  }) }) }) }))))((_m as any).value);
    default: return (() => {
  const mkAlias = ((styp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "type", value: ({
    mods: [],
    name: ({
    value: lname
  }),
    tparams: tparams,
    body: styp
  }) }) }) }));
  return LibEithers.either(((_: Errors.Error) => mkAlias(ScalaUtils.stref("Any"))))(mkAlias)(encodeType(cx)(g)(typ));
})()(_m);
  }
})();
})()));
}

export function encodeTypedParam<t0, t1>(cx: t0): ((x: t1) => ((x: readonly [Core.Name, Core.Type]) => Errors.Error | ScalaSyntax.Data_Param)) {
  return ((g: t1) => ((pair: readonly [Core.Name, Core.Type]) => (() => {
  const pname = ScalaUtils.scalaEscapeName(Names.localNameOf(LibPairs.first(pair)));
  const pdom = LibPairs.second(pair);
  return LibEithers.bind(encodeType(cx)(g)(pdom))(((sdom: ScalaSyntax.Type) => ({ tag: "right", value: ({
    mods: [],
    name: ({ tag: "value", value: pname }),
    decltpe: sdom,
    default: null
  }) })));
})()));
}

export function encodeUntypeApplicationTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | ScalaSyntax.Data)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.bind(Inference.inferInGraphContext(cx)(g)(term))(((result: Typing.InferenceResult) => encodeTerm(cx)(g)(((_x) => _x.term)(result))))));
}

export function extractBody(t: Core.Term): Core.Term {
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => extractBody(((_x) => _x.body)(lam)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => extractBody(((_x) => _x.body)(tl)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => extractBody(((_x) => _x.body)(ta)))((_m as any).value);
    case "let": return ((lt: Core.Let) => extractBody(((_x) => _x.body)(lt)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function extractCodomain(t: Core.Type): Core.Type {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => extractCodomain(((_x) => _x.codomain)(ft)))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => extractCodomain(((_x) => _x.body)(fa)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function extractDomains(t: Core.Type): ReadonlyArray<Core.Type> {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => LibLists.cons(((_x) => _x.domain)(ft))(extractDomains(((_x) => _x.codomain)(ft))))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => extractDomains(((_x) => _x.body)(fa)))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function extractLetBindings(t: Core.Term): ReadonlyArray<Core.Binding> {
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => extractLetBindings(((_x) => _x.body)(lam)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => extractLetBindings(((_x) => _x.body)(tl)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => extractLetBindings(((_x) => _x.body)(ta)))((_m as any).value);
    case "let": return ((lt: Core.Let) => LibLists.concat2(((_x) => _x.bindings)(lt))(extractLetBindings(((_x) => _x.body)(lt))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function extractParams(t: Core.Term): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibLists.cons(((_x) => _x.parameter)(lam))(extractParams(((_x) => _x.body)(lam))))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => extractParams(((_x) => _x.body)(tl)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => extractParams(((_x) => _x.body)(ta)))((_m as any).value);
    case "let": return ((lt: Core.Let) => extractParams(((_x) => _x.body)(lt)))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function fieldToEnumCase<t0, t1>(cx: t0): ((x: t1) => ((x: string) => ((x: ReadonlyArray<ScalaSyntax.Type_Param>) => ((x: Core.FieldType) => Errors.Error | ScalaSyntax.Stat)))) {
  return ((g: t1) => ((parentName: string) => ((tparams: ReadonlyArray<ScalaSyntax.Type_Param>) => ((ft: Core.FieldType) => (() => {
  const fname = ScalaUtils.scalaEscapeName(((_x) => _x)(((_x) => _x.name)(ft)));
  const ftyp = ((_x) => _x.type)(ft);
  const caseName = ({
    value: fname
  });
  const isUnit = (() => {
  const _m = Strip.deannotateType(ftyp);
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEquality.equal(LibLists.length(rt))(0))((_m as any).value);
    default: return false(_m);
  }
})();
  const parentType = LibLogic.ifElse(LibLists.null_(tparams))(({ tag: "ref", value: ({ tag: "name", value: ({
    value: parentName
  }) }) }))(({ tag: "apply", value: ({
    tpe: ({ tag: "ref", value: ({ tag: "name", value: ({
    value: parentName
  }) }) }),
    args: LibLists.map(typeParamToTypeVar)(tparams)
  }) }));
  return LibEithers.bind(encodeType(cx)(g)(ftyp))(((sftyp: ScalaSyntax.Type) => ({ tag: "right", value: ({ tag: "defn", value: ({ tag: "enumCase", value: ({
    mods: [],
    name: caseName,
    tparams: [],
    ctor: ({
    mods: [],
    name: ({ tag: "value", value: "" }),
    paramss: [LibLogic.ifElse(isUnit)([])([({
    mods: [],
    name: ({ tag: "value", value: "value" }),
    decltpe: sftyp,
    default: null
  })])]
  }),
    inits: [({
    tpe: parentType,
    name: ({ tag: "value", value: "" }),
    argss: []
  })]
  }) }) }) })));
})()))));
}

export function fieldToParam<t0, t1>(cx: t0): ((x: t1) => ((x: Core.FieldType) => Errors.Error | ScalaSyntax.Data_Param)) {
  return ((g: t1) => ((ft: Core.FieldType) => (() => {
  const fname = ScalaUtils.scalaEscapeName(((_x) => _x)(((_x) => _x.name)(ft)));
  const ftyp = ((_x) => _x.type)(ft);
  return LibEithers.bind(encodeType(cx)(g)(ftyp))(((sftyp: ScalaSyntax.Type) => ({ tag: "right", value: ({
    mods: [],
    name: ({ tag: "value", value: fname }),
    decltpe: sftyp,
    default: null
  }) })));
})()));
}

export function findDomain<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | Core.Type)) {
  return ((g: Graph.Graph) => ((meta: ReadonlyMap<Core.Name, Core.Term>) => LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(meta)))(((r: Core.Type | null) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: "expected a typed term" }) }))(((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => ({ tag: "right", value: ((_x) => _x.domain)(ft) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "expected a function type" }) })(_m);
  }
})()))(r)))));
}

export function findImports<t0>(cx: t0): ((x: Graph.Graph) => ((x: Packaging.Module) => Errors.Error | ReadonlyArray<ScalaSyntax.Stat>)) {
  return ((g: Graph.Graph) => ((mod: Packaging.Module) => LibEithers.bind(Analysis.moduleDependencyNamespaces(cx)(g)(false)(false)(true)(false)(mod))(((elImps: ReadonlySet<Packaging.Namespace>) => LibEithers.bind(Analysis.moduleDependencyNamespaces(cx)(g)(false)(true)(false)(false)(mod))(((primImps: ReadonlySet<Packaging.Namespace>) => ({ tag: "right", value: LibLists.concat([LibLists.map(toElImport)(LibSets.toList(elImps)), LibLists.map(toPrimImport)(LibSets.toList(primImps))]) })))))));
}

export function findSdom<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyMap<Core.Name, Core.Term>) => Errors.Error | ScalaSyntax.Type | null)) {
  return ((g: Graph.Graph) => ((meta: ReadonlyMap<Core.Name, Core.Term>) => LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(meta)))(((mtyp: Core.Type | null) => LibMaybes.maybe(({ tag: "right", value: null }))(((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(ft);
  return LibEithers.bind(encodeType(cx)(g)(dom))(((sdom: ScalaSyntax.Type) => ({ tag: "right", value: sdom })));
})())((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.body)(fa));
  switch (_m.tag) {
    case "function": return ((ft2: Core.FunctionType) => (() => {
  const dom2 = ((_x) => _x.domain)(ft2);
  return LibEithers.bind(encodeType(cx)(g)(dom2))(((sdom2: ScalaSyntax.Type) => ({ tag: "right", value: sdom2 })));
})())((_m as any).value);
    default: return ({ tag: "right", value: null })(_m);
  }
})())((_m as any).value);
    default: return LibEithers.bind(encodeType(cx)(g)(t))(((st: ScalaSyntax.Type) => ({ tag: "right", value: st })))(_m);
  }
})()))(mtyp)))));
}

export function moduleToScala<t0>(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<string, string>))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(constructModule(cx)(g)(mod)(defs))(((pkg: ScalaSyntax.Pkg) => (() => {
  const s = Serialization.printExpr(Serialization.parenthesize(ScalaSerde.writePkg(pkg)));
  return ({ tag: "right", value: LibMaps.singleton(Names.namespaceToFilePath(({ tag: "camel" }))("scala")(((_x) => _x.namespace)(mod)))(s) });
})())))));
}

export function stripWrapEliminations(t: Core.Term): Core.Term {
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const appFun = ((_x) => _x.function)(app);
  const appArg = ((_x) => _x.argument)(app);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(appFun);
  switch (_m.tag) {
    case "unwrap": return ((_: Core.Name) => stripWrapEliminations(appArg))((_m as any).value);
    case "application": return ((innerApp: Core.Application) => (() => {
  const innerFun = ((_x) => _x.function)(innerApp);
  const innerArg = ((_x) => _x.argument)(innerApp);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(innerFun);
  switch (_m.tag) {
    case "unwrap": return ((_: Core.Name) => stripWrapEliminations(({ tag: "application", value: ({
    function: innerArg,
    argument: appArg
  }) })))((_m as any).value);
    default: return t(_m);
  }
})();
})())((_m as any).value);
    default: return t(_m);
  }
})();
})())((_m as any).value);
    default: return t(_m);
  }
})();
}

export function toElImport(ns: Packaging.Namespace): ScalaSyntax.Stat {
  return ({ tag: "importExport", value: ({ tag: "import", value: ({
    importers: [({
    ref: ({ tag: "name", value: ({
    value: LibStrings.intercalate(".")(LibStrings.splitOn(".")(((_x) => _x)(ns)))
  }) }),
    importees: [({ tag: "wildcard" })]
  })]
  }) }) });
}

export function toPrimImport(ns: Packaging.Namespace): ScalaSyntax.Stat {
  return ({ tag: "importExport", value: ({ tag: "import", value: ({
    importers: [({
    ref: ({ tag: "name", value: ({
    value: LibStrings.intercalate(".")(LibStrings.splitOn(".")(((_x) => _x)(ns)))
  }) }),
    importees: []
  })]
  }) }) });
}

export function typeParamToTypeVar(tp: ScalaSyntax.Type_Param): ScalaSyntax.Type {
  return (() => {
  const n = ((_x) => _x.name)(tp);
  const s = (() => {
  const _m = n;
  switch (_m.tag) {
    case "value": return ((v: string) => v)((_m as any).value);
    default: return ""(_m);
  }
})();
  return ({ tag: "var", value: ({
    name: ({
    value: s
  })
  }) });
})();
}
