// Note: this is an automatically generated file. Do not edit.

/**
 * Lisp code generator: converts Hydra type and term modules to Lisp AST
 */



import * as Analysis from "../analysis.js";
import * as Ast from "../ast.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Environment from "../environment.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as JsonModel from "../json/model.js";
import * as Lexical from "../lexical.js";
import * as LibEithers from "../lib/eithers.js";
import * as LibEquality from "../lib/equality.js";
import * as LibLists from "../lib/lists.js";
import * as LibLiterals from "../lib/literals.js";
import * as LibLogic from "../lib/logic.js";
import * as LibMaps from "../lib/maps.js";
import * as LibMaybes from "../lib/maybes.js";
import * as LibPairs from "../lib/pairs.js";
import * as LibSets from "../lib/sets.js";
import * as LibStrings from "../lib/strings.js";
import * as LispLanguage from "./language.js";
import * as LispSyntax from "./syntax.js";
import * as Names from "../names.js";
import * as Packaging from "../packaging.js";
import * as Parsing from "../parsing.js";
import * as Paths from "../paths.js";
import * as Phantoms from "../phantoms.js";
import * as Predicates from "../predicates.js";
import * as Query from "../query.js";
import * as Relational from "../relational.js";
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

export function dialectCadr(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "second")((_m as any).value);
    default: return "cadr"(_m);
  }
})();
}

export function dialectCar(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "first")((_m as any).value);
    default: return "car"(_m);
  }
})();
}

export function dialectConstructorPrefix(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "->")((_m as any).value);
    default: return "make-"(_m);
  }
})();
}

export function dialectEqual(d: LispSyntax.Dialect): string {
  return (() => {
  const _m = d;
  switch (_m.tag) {
    case "clojure": return ((_: void) => "=")((_m as any).value);
    case "commonLisp": return ((_: void) => "equal")((_m as any).value);
    case "emacsLisp": return ((_: void) => "equal")((_m as any).value);
    default: return "equal?"(_m);
  }
})();
}

export function encodeApplication<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.Term) => ((x: Core.Term) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((rawFun: Core.Term) => ((rawArg: Core.Term) => (() => {
  const dFun = Strip.deannotateTerm(rawFun);
  return (() => {
  const normal = LibEithers.bind(encodeTerm(dialect)(cx)(g)(rawFun))(((fun: LispSyntax.Expression) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(rawArg))(((arg: LispSyntax.Expression) => ({ tag: "right", value: lispApp(fun)([arg]) })))));
  return (() => {
  const enc = ((t: Core.Term) => encodeTerm(dialect)(cx)(g)(t));
  return (() => {
  const _m = dFun;
  switch (_m.tag) {
    case "application": return ((app2: Core.Application) => (() => {
  const midFun = ((_x) => _x.function)(app2);
  return (() => {
  const midArg = ((_x) => _x.argument)(app2);
  return (() => {
  const dMidFun = Strip.deannotateTerm(midFun);
  return (() => {
  const isLazy2 = LibLogic.or(isPrimitiveRef("hydra.lib.eithers.fromLeft")(dMidFun))(LibLogic.or(isPrimitiveRef("hydra.lib.eithers.fromRight")(dMidFun))(isPrimitiveRef("hydra.lib.maybes.fromMaybe")(dMidFun)));
  return LibLogic.ifElse(isLazy2)(LibEithers.bind(enc(midFun))(((ePrim: LispSyntax.Expression) => LibEithers.bind(enc(midArg))(((eDef: LispSyntax.Expression) => LibEithers.bind(enc(rawArg))(((eArg: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispApp(ePrim)([wrapInThunk(eDef)]))([eArg]) }))))))))((() => {
  const _m = dMidFun;
  switch (_m.tag) {
    case "application": return ((app3: Core.Application) => (() => {
  const innerFun = ((_x) => _x.function)(app3);
  return (() => {
  const innerArg = ((_x) => _x.argument)(app3);
  return (() => {
  const dInnerFun = Strip.deannotateTerm(innerFun);
  return LibLogic.ifElse(isPrimitiveRef("hydra.lib.logic.ifElse")(dInnerFun))(LibEithers.bind(enc(innerArg))(((eC: LispSyntax.Expression) => LibEithers.bind(enc(midArg))(((eT: LispSyntax.Expression) => LibEithers.bind(enc(rawArg))(((eE: LispSyntax.Expression) => ({ tag: "right", value: ({ tag: "if", value: ({
    condition: eC,
    then: eT,
    else: eE
  }) }) }))))))))(LibLogic.ifElse(isPrimitiveRef("hydra.lib.maybes.maybe")(dInnerFun))(LibEithers.bind(enc(innerFun))(((eP: LispSyntax.Expression) => LibEithers.bind(enc(innerArg))(((eDef: LispSyntax.Expression) => LibEithers.bind(enc(midArg))(((eF: LispSyntax.Expression) => LibEithers.bind(enc(rawArg))(((eM: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispApp(lispApp(eP)([wrapInThunk(eDef)]))([eF]))([eM]) }))))))))))(LibLogic.ifElse(isPrimitiveRef("hydra.lib.maybes.cases")(dInnerFun))(LibEithers.bind(enc(innerFun))(((eP: LispSyntax.Expression) => LibEithers.bind(enc(innerArg))(((eM: LispSyntax.Expression) => LibEithers.bind(enc(midArg))(((eN: LispSyntax.Expression) => LibEithers.bind(enc(rawArg))(((eJ: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispApp(lispApp(eP)([eM]))([wrapInThunk(eN)]))([eJ]) }))))))))))(normal)));
})();
})();
})())((_m as any).value);
    default: return normal(_m);
  }
})());
})();
})();
})();
})())((_m as any).value);
    default: return normal(_m);
  }
})();
})();
})();
})()))));
}

export function encodeFieldDef(ft: Core.FieldType): LispSyntax.FieldDefinition {
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(ft));
  return ({
    name: Formatting.convertCaseCamelToLowerSnake(fname),
    defaultValue: null
  });
})();
}

export function encodeLambdaTerm<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.Lambda) => t2 | LispSyntax.Expression))) {
  return ((cx: t0) => ((g: t1) => ((lam: Core.Lambda) => (() => {
  const param = Formatting.convertCaseCamelOrUnderscoreToLowerSnake(Formatting.sanitizeWithUnderscores(LispLanguage.lispReservedWords)(((_x) => _x)(((_x) => _x.parameter)(lam))));
  return LibEithers.bind(encodeTerm(dialect)(cx)(g)(((_x) => _x.body)(lam)))(((body: LispSyntax.Expression) => ({ tag: "right", value: lispLambdaExpr([param])(body) })));
})())));
}

export function encodeLetAsLambdaApp<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: ReadonlyArray<Core.Binding>) => ((x: Core.Term) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((bindings: ReadonlyArray<Core.Binding>) => ((body: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(body))(((bodyExpr: LispSyntax.Expression) => LibEithers.foldl(((acc: LispSyntax.Expression) => ((b: Core.Binding) => (() => {
  const bname = Formatting.convertCaseCamelOrUnderscoreToLowerSnake(Formatting.sanitizeWithUnderscores(LispLanguage.lispReservedWords)(((_x) => _x)(((_x) => _x.name)(b))));
  return LibEithers.bind(encodeTerm(dialect)(cx)(g)(((_x) => _x.term)(b)))(((bval: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispLambdaExpr([bname])(acc))([bval]) })));
})())))(bodyExpr)(LibLists.reverse(bindings))))))));
}

export function encodeLetAsNative<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: ReadonlyArray<Core.Binding>) => ((x: Core.Term) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((bindings: ReadonlyArray<Core.Binding>) => ((body: Core.Term) => (() => {
  const isClojureTop = (() => {
  const _m = dialect;
  switch (_m.tag) {
    case "clojure": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  return LibEithers.bind(encodeTerm(dialect)(cx)(g)(body))(((bodyExpr: LispSyntax.Expression) => (() => {
  const sortedBindings = LibLogic.ifElse(true)((() => {
  const allNames = LibSets.fromList(LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(bindings));
  const adjList = LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), LibSets.toList(LibSets.intersection(allNames)(Variables.freeVariablesInTerm(((_x) => _x.term)(b))))]))(bindings);
  const sortResult = Sorting.topologicalSort(adjList);
  const nameToBinding = LibMaps.fromList(LibLists.map(((b: Core.Binding) => [((_x) => _x.name)(b), b]))(bindings));
  return LibEithers.either(((_: ReadonlyArray<ReadonlyArray<Core.Name>>) => bindings))(((sorted: ReadonlyArray<Core.Name>) => LibLists.map(((name: Core.Name) => LibMaybes.fromMaybe(LibLists.head(bindings))(LibMaps.lookup(name)(nameToBinding))))(sorted)))(sortResult);
})())(bindings);
  return LibEithers.bind(LibEithers.mapList(((b: Core.Binding) => (() => {
  const bname = Formatting.convertCaseCamelOrUnderscoreToLowerSnake(Formatting.sanitizeWithUnderscores(LispLanguage.lispReservedWords)(((_x) => _x)(((_x) => _x.name)(b))));
  return (() => {
  const isSelfRef = LibSets.member(((_x) => _x.name)(b))(Variables.freeVariablesInTerm(((_x) => _x.term)(b)));
  return (() => {
  const isLambda = (() => {
  const _m = Strip.deannotateTerm(((_x) => _x.term)(b));
  switch (_m.tag) {
    case "lambda": return ((_: Core.Lambda) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  return LibEithers.bind(encodeTerm(dialect)(cx)(g)(((_x) => _x.term)(b)))(((bval: LispSyntax.Expression) => (() => {
  const isClojure = (() => {
  const _m = dialect;
  switch (_m.tag) {
    case "clojure": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})();
  return (() => {
  const wrappedVal = LibLogic.ifElse(isClojure)(LibLogic.ifElse(isSelfRef)(LibLogic.ifElse(isLambda)((() => {
  const _m = bval;
  switch (_m.tag) {
    case "lambda": return ((lam: LispSyntax.Lambda) => ({ tag: "lambda", value: ({
    name: bname,
    params: ((_x) => _x.params)(lam),
    restParam: ((_x) => _x.restParam)(lam),
    body: ((_x) => _x.body)(lam)
  }) }))((_m as any).value);
    default: return bval(_m);
  }
})())(lispNamedLambdaExpr(bname)(["_arg"])(lispApp(bval)([lispVar("_arg")]))))(bval))(LibLogic.ifElse(LibLogic.and(isSelfRef)(LibLogic.not(isLambda)))(lispLambdaExpr(["_arg"])(lispApp(bval)([lispVar("_arg")])))(bval));
  return ({ tag: "right", value: [bname, wrappedVal] });
})();
})()));
})();
})();
})()))(sortedBindings))(((encodedBindings: ReadonlyArray<readonly [string, LispSyntax.Expression]>) => (() => {
  const allBindingNames = LibSets.fromList(LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(bindings));
  return (() => {
  const hasCrossRefs = LibLists.foldl(((acc: boolean) => ((b: Core.Binding) => LibLogic.or(acc)(LibLogic.not(LibSets.null_(LibSets.intersection(allBindingNames)(Variables.freeVariablesInTerm(((_x) => _x.term)(b)))))))))(false)(bindings);
  return (() => {
  const hasSelfRef = LibLists.foldl(((acc: boolean) => ((b: Core.Binding) => LibLogic.or(acc)(LibSets.member(((_x) => _x.name)(b))(Variables.freeVariablesInTerm(((_x) => _x.term)(b)))))))(false)(bindings);
  return (() => {
  const isRecursive = hasSelfRef;
  return (() => {
  const letKind = LibLogic.ifElse(isRecursive)(({ tag: "recursive" }))(LibLogic.ifElse(LibLists.null_(LibLists.tail(bindings)))(({ tag: "parallel" }))(({ tag: "sequential" })));
  return (() => {
  const lispBindings = LibLists.map(((eb: readonly [string, LispSyntax.Expression]) => ({ tag: "simple", value: ({
    name: LibPairs.first(eb),
    value: LibPairs.second(eb)
  }) })))(encodedBindings);
  return ({ tag: "right", value: ({ tag: "let", value: ({
    kind: letKind,
    bindings: lispBindings,
    body: [bodyExpr]
  }) }) });
})();
})();
})();
})();
})();
})()));
})()));
})()))));
}

export function encodeLiteral(lit: Core.Literal): LispSyntax.Expression {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "boolean": return ((b: boolean) => ({ tag: "literal", value: ({ tag: "boolean", value: b }) }))((_m as any).value);
    case "string": return ((s: string) => ({ tag: "literal", value: ({ tag: "string", value: s }) }))((_m as any).value);
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "float32": return ((f: number) => ({ tag: "literal", value: ({ tag: "float", value: ({
    value: LibLiterals.float32ToBigfloat(f),
    precision: null
  }) }) }))((_m as any).value);
    case "float64": return ((f: number) => ({ tag: "literal", value: ({ tag: "float", value: ({
    value: LibLiterals.float64ToBigfloat(f),
    precision: null
  }) }) }))((_m as any).value);
    case "bigfloat": return ((f: number) => ({ tag: "literal", value: ({ tag: "float", value: ({
    value: f,
    precision: null
  }) }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((iv: Core.IntegerValue) => (() => {
  const _m = iv;
  switch (_m.tag) {
    case "int8": return ((i: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.int8ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "int16": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.int16ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "int32": return ((i: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.int32ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "int64": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.int64ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "uint8": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.uint8ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "uint16": return ((i: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.uint16ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "uint32": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.uint32ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "uint64": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.uint64ToBigint(i),
    bigint: false
  }) }) }))((_m as any).value);
    case "bigint": return ((i: bigint) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: i,
    bigint: true
  }) }) }))((_m as any).value);
  }
})())((_m as any).value);
    case "binary": return ((b: Uint8Array) => (() => {
  const byteValues = LibLiterals.binaryToBytes(b);
  return ({ tag: "vector", value: ({
    elements: LibLists.map(((bv: number) => ({ tag: "literal", value: ({ tag: "integer", value: ({
    value: LibLiterals.int32ToBigint(bv),
    bigint: false
  }) }) })))(byteValues)
  }) });
})())((_m as any).value);
  }
})();
}

export function encodeProjectionElim<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.Projection) => ((x: Core.Term | null) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((proj: Core.Projection) => ((marg: Core.Term | null) => (() => {
  const fname = Formatting.convertCaseCamelToLowerSnake(((_x) => _x)(((_x) => _x.field)(proj)));
  return (() => {
  const tname = qualifiedSnakeName(((_x) => _x.typeName)(proj));
  return LibMaybes.cases(marg)(({ tag: "right", value: lispLambdaExpr(["v"])(({ tag: "fieldAccess", value: ({
    recordType: tname,
    field: fname,
    target: lispVar("v")
  }) })) }))(((arg: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(arg))(((sarg: LispSyntax.Expression) => ({ tag: "right", value: ({ tag: "fieldAccess", value: ({
    recordType: tname,
    field: fname,
    target: sarg
  }) }) })))));
})();
})()))));
}

export function encodeTerm<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.Term) => t2 | LispSyntax.Expression))) {
  return ((cx: t0) => ((g: t1) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => encodeTerm(dialect)(cx)(g)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const rawFun = ((_x) => _x.function)(app);
  return (() => {
  const rawArg = ((_x) => _x.argument)(app);
  return encodeApplication(dialect)(cx)(g)(rawFun)(rawArg);
})();
})())((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((l: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(l))(((sl: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword("left"), sl]) })))))(((r: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(r))(((sr: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword("right"), sr]) })))))(e))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => encodeLambdaTerm(dialect)(cx)(g)(lam))((_m as any).value);
    case "project": return ((proj: Core.Projection) => encodeProjectionElim(dialect)(cx)(g)(proj)(null))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => encodeUnionElim(dialect)(cx)(g)(cs)(null))((_m as any).value);
    case "unwrap": return ((name: Core.Name) => encodeUnwrapElim(dialect)(cx)(g)(name)(null))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return encodeLetAsNative(dialect)(cx)(g)(bindings)(body);
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(((v1: Core.Term) => encodeTerm(dialect)(cx)(g)(v1)))(els))(((sels: ReadonlyArray<LispSyntax.Expression>) => ({ tag: "right", value: lispListExpr(sels) }))))((_m as any).value);
    case "literal": return ((lit: Core.Literal) => ({ tag: "right", value: encodeLiteral(lit) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.bind(LibEithers.mapList(((entry: readonly [Core.Term, Core.Term]) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(LibPairs.first(entry)))(((k: LispSyntax.Expression) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(LibPairs.second(entry)))(((v: LispSyntax.Expression) => ({ tag: "right", value: ({
    key: k,
    value: v
  }) })))))))(LibMaps.toList(m)))(((pairs: ReadonlyArray<LispSyntax.MapEntry>) => ({ tag: "right", value: ({ tag: "map", value: ({
    entries: pairs
  }) }) }))))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.cases(mt)(({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword("nothing")]) }))(((val: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(val))(((sval: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword("just"), sval]) }))))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(LibPairs.first(p)))(((f: LispSyntax.Expression) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(LibPairs.second(p)))(((s: LispSyntax.Expression) => ({ tag: "right", value: lispListExpr([f, s]) }))))))((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const rname = ((_x) => _x.typeName)(rec);
  return (() => {
  const fields = ((_x) => _x.fields)(rec);
  return LibEithers.bind(LibEithers.mapList(((f: Core.Field) => encodeTerm(dialect)(cx)(g)(((_x) => _x.term)(f))))(fields))(((sfields: ReadonlyArray<LispSyntax.Expression>) => (() => {
  const constructorName = LibStrings.cat2(dialectConstructorPrefix(dialect))(qualifiedSnakeName(rname));
  return ({ tag: "right", value: lispApp(lispVar(constructorName))(sfields) });
})()));
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.bind(LibEithers.mapList(((v1: Core.Term) => encodeTerm(dialect)(cx)(g)(v1)))(LibSets.toList(s)))(((sels: ReadonlyArray<LispSyntax.Expression>) => ({ tag: "right", value: ({ tag: "set", value: ({
    elements: sels
  }) }) }))))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const tname = Names.localNameOf(((_x) => _x.typeName)(inj));
  return (() => {
  const field = ((_x) => _x.field)(inj);
  return (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(field));
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const dterm = Strip.deannotateTerm(fterm);
  return (() => {
  const isUnit = (() => {
  const _m = dterm;
  switch (_m.tag) {
    case "unit": return ((_: void) => true)((_m as any).value);
    case "record": return ((rt: Core.Record) => LibLists.null_(((_x) => _x.fields)(rt)))((_m as any).value);
    default: return false(_m);
  }
})();
  return LibLogic.ifElse(isUnit)(({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword(Formatting.convertCaseCamelToLowerSnake(fname)), lispNilExpr]) }))(LibEithers.bind(encodeTerm(dialect)(cx)(g)(fterm))(((sval: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispVar("list"))([lispKeyword(Formatting.convertCaseCamelToLowerSnake(fname)), sval]) }))));
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: lispNilExpr }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "right", value: lispVar(Formatting.convertCaseCamelOrUnderscoreToLowerSnake(Formatting.sanitizeWithUnderscores(LispLanguage.lispReservedWords)(((_x) => _x)(name)))) }))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => encodeTerm(dialect)(cx)(g)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => encodeTerm(dialect)(cx)(g)(((_x) => _x.body)(tl)))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => encodeTerm(dialect)(cx)(g)(((_x) => _x.body)(wt)))((_m as any).value);
  }
})())));
}

export function encodeTermDefinition<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Packaging.TermDefinition) => t2 | LispSyntax.TopLevelFormWithComments))) {
  return ((cx: t0) => ((g: t1) => ((tdef: Packaging.TermDefinition) => (() => {
  const name = ((_x) => _x.name)(tdef);
  return (() => {
  const term = ((_x) => _x.term)(tdef);
  return (() => {
  const lname = qualifiedSnakeName(name);
  return (() => {
  const dterm = Strip.deannotateTerm(term);
  return (() => {
  const _m = dterm;
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(term))(((sterm: LispSyntax.Expression) => ({ tag: "right", value: lispTopForm(({ tag: "variable", value: ({
    name: lname,
    value: sterm,
    doc: null
  }) })) }))))((_m as any).value);
    default: return LibEithers.bind(encodeTerm(dialect)(cx)(g)(term))(((sterm: LispSyntax.Expression) => ({ tag: "right", value: lispTopForm(({ tag: "variable", value: ({
    name: lname,
    value: sterm,
    doc: null
  }) })) })))(_m);
  }
})();
})();
})();
})();
})())));
}

export function encodeType<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Core.Type) => t2 | LispSyntax.TypeSpecifier)) {
  return ((g: t1) => ((t: Core.Type) => (() => {
  const typ = Strip.deannotateType(t);
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => encodeType(cx)(g)(((_x) => _x.body)(at)))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => encodeType(cx)(g)(((_x) => _x.function)(at)))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: ({ tag: "unit" }) }))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => ({ tag: "right", value: (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "named", value: "ByteArray" }))((_m as any).value);
    case "boolean": return ((_: void) => ({ tag: "named", value: "Boolean" }))((_m as any).value);
    case "float": return ((_: Core.FloatType) => ({ tag: "named", value: "Float" }))((_m as any).value);
    case "integer": return ((_: Core.IntegerType) => ({ tag: "named", value: "Integer" }))((_m as any).value);
    case "string": return ((_: void) => ({ tag: "named", value: "String" }))((_m as any).value);
  }
})() }))((_m as any).value);
    case "list": return ((inner: Core.Type) => LibEithers.map(((enc: LispSyntax.TypeSpecifier) => ({ tag: "list", value: enc })))(encodeType(cx)(g)(inner)))((_m as any).value);
    case "set": return ((inner: Core.Type) => LibEithers.map(((enc: LispSyntax.TypeSpecifier) => ({ tag: "set", value: enc })))(encodeType(cx)(g)(inner)))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "right", value: ({ tag: "named", value: "Map" }) }))((_m as any).value);
    case "maybe": return ((inner: Core.Type) => LibEithers.map(((enc: LispSyntax.TypeSpecifier) => ({ tag: "maybe", value: enc })))(encodeType(cx)(g)(inner)))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "right", value: ({ tag: "named", value: "Either" }) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "right", value: ({ tag: "named", value: "Pair" }) }))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => ({ tag: "right", value: ({ tag: "named", value: "Function" }) }))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: ({ tag: "named", value: "Record" }) }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: ({ tag: "named", value: "Union" }) }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "right", value: ({ tag: "named", value: "Wrapper" }) }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "right", value: ({ tag: "named", value: ((_x) => _x)(name) }) }))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => encodeType(cx)(g)(((_x) => _x.body)(fa)))((_m as any).value);
    default: return ({ tag: "right", value: ({ tag: "named", value: "Any" }) })(_m);
  }
})();
})()));
}

export function encodeTypeBody<t0>(lname: string): ((x: Core.Type) => ((x: Core.Type) => t0 | LispSyntax.TopLevelFormWithComments)) {
  return ((origTyp: Core.Type) => ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => encodeTypeBody(lname)(origTyp)(((_x) => _x.body)(ft)))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const fields = LibLists.map(encodeFieldDef)(rt);
  return ({ tag: "right", value: lispTopForm(({ tag: "recordType", value: ({
    name: lname,
    fields: fields,
    doc: null
  }) })) });
})())((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const variantNames = LibLists.map(((f: Core.FieldType) => ({ tag: "literal", value: ({ tag: "keyword", value: ({
    name: Formatting.convertCaseCamelToLowerSnake(((_x) => _x)(((_x) => _x.name)(f))),
    namespace: null
  }) }) })))(rt);
  return ({ tag: "right", value: lispTopForm(({ tag: "variable", value: ({
    name: LibStrings.cat2(lname)("-variants"),
    value: lispListExpr(variantNames),
    doc: LibStrings.cat2("Variants of the ")(lname)
  }) })) });
})())((_m as any).value);
    case "wrap": return ((wt: Core.Type) => ({ tag: "right", value: lispTopForm(({ tag: "recordType", value: ({
    name: lname,
    fields: [({
    name: "value",
    defaultValue: null
  })],
    doc: null
  }) })) }))((_m as any).value);
    default: return ({ tag: "right", value: ({
    doc: null,
    comment: ({
    style: ({ tag: "line" }),
    text: LibStrings.cat2(LibStrings.cat2(lname)(" = "))(ShowCore.type(origTyp))
  }),
    form: ({ tag: "expression", value: ({ tag: "literal", value: ({ tag: "nil" }) }) })
  }) })(_m);
  }
})()));
}

export function encodeTypeDefinition<t0, t1, t2>(cx: t0): ((x: t1) => ((x: Packaging.TypeDefinition) => t2 | LispSyntax.TopLevelFormWithComments)) {
  return ((g: t1) => ((tdef: Packaging.TypeDefinition) => (() => {
  const name = ((_x) => _x.name)(tdef);
  return (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(tdef));
  return (() => {
  const lname = qualifiedSnakeName(name);
  return (() => {
  const dtyp = Strip.deannotateType(typ);
  return encodeTypeBody(lname)(typ)(dtyp);
})();
})();
})();
})()));
}

export function encodeUnionElim<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.CaseStatement) => ((x: Core.Term | null) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((cs: Core.CaseStatement) => ((marg: Core.Term | null) => (() => {
  const tname = Names.localNameOf(((_x) => _x.typeName)(cs));
  return (() => {
  const caseFields = ((_x) => _x.cases)(cs);
  return (() => {
  const defCase = ((_x) => _x.default)(cs);
  return LibEithers.bind(LibEithers.mapList(((cf: Core.Field) => (() => {
  const cfname = Formatting.convertCaseCamelToLowerSnake(((_x) => _x)(((_x) => _x.name)(cf)));
  return (() => {
  const cfterm = ((_x) => _x.term)(cf);
  return (() => {
  const condExpr = lispApp(lispVar(dialectEqual(dialect)))([lispApp(lispVar(dialectCar(dialect)))([lispVar("match_target")]), lispKeyword(cfname)]);
  return LibEithers.bind(encodeTerm(dialect)(cx)(g)(({ tag: "application", value: ({
    function: cfterm,
    argument: ({ tag: "variable", value: "match_value" })
  }) })))(((bodyExpr: LispSyntax.Expression) => ({ tag: "right", value: ({
    condition: condExpr,
    body: bodyExpr
  }) })));
})();
})();
})()))(caseFields))(((clauses: ReadonlyArray<LispSyntax.CondClause>) => LibEithers.bind(LibMaybes.cases(defCase)(({ tag: "right", value: null }))(((dt: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(dt))(((defBody: LispSyntax.Expression) => ({ tag: "right", value: defBody }))))))(((defExpr: LispSyntax.Expression | null) => (() => {
  const condExpr = ({ tag: "cond", value: ({
    clauses: clauses,
    default: defExpr
  }) });
  return (() => {
  const innerExpr = lispApp(lispLambdaExpr(["match_value"])(condExpr))([lispApp(lispVar(dialectCadr(dialect)))([lispVar("match_target")])]);
  return LibMaybes.cases(marg)(({ tag: "right", value: lispLambdaExpr(["match_target"])(innerExpr) }))(((arg: Core.Term) => LibEithers.bind(encodeTerm(dialect)(cx)(g)(arg))(((sarg: LispSyntax.Expression) => ({ tag: "right", value: lispApp(lispLambdaExpr(["match_target"])(innerExpr))([sarg]) })))));
})();
})()))));
})();
})();
})()))));
}

export function encodeUnwrapElim<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: t0) => ((x: t1) => ((x: Core.Name) => ((x: Core.Term | null) => t2 | LispSyntax.Expression)))) {
  return ((cx: t0) => ((g: t1) => ((name: Core.Name) => ((marg: Core.Term | null) => LibMaybes.cases(marg)(({ tag: "right", value: lispLambdaExpr(["v"])(lispVar("v")) }))(((arg: Core.Term) => encodeTerm(dialect)(cx)(g)(arg)))))));
}

export function isCasesPrimitive(name: Core.Name): boolean {
  return LibEquality.equal(name)("hydra.lib.maybes.cases");
}

export function isLazy2ArgPrimitive(name: Core.Name): boolean {
  return LibLogic.or(LibEquality.equal(name)("hydra.lib.eithers.fromLeft"))(LibLogic.or(LibEquality.equal(name)("hydra.lib.eithers.fromRight"))(LibEquality.equal(name)("hydra.lib.maybes.fromMaybe")));
}

export function isLazy3ArgPrimitive(name: Core.Name): boolean {
  return LibEquality.equal(name)("hydra.lib.maybes.maybe");
}

export function isPrimitiveRef(primName: string): ((x: Core.Term) => boolean) {
  return ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => LibEquality.equal(((_x) => _x)(name))(primName))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedTerm) => isPrimitiveRef(primName)(((_x) => _x.body)(at)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => isPrimitiveRef(primName)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => isPrimitiveRef(primName)(((_x) => _x.body)(tl)))((_m as any).value);
    default: return false(_m);
  }
})());
}

export function lispApp(fun: LispSyntax.Expression): ((x: ReadonlyArray<LispSyntax.Expression>) => LispSyntax.Expression) {
  return ((args: ReadonlyArray<LispSyntax.Expression>) => ({ tag: "application", value: ({
    function: fun,
    arguments: args
  }) }));
}

export function lispKeyword(name: string): LispSyntax.Expression {
  return ({ tag: "literal", value: ({ tag: "keyword", value: ({
    name: name,
    namespace: null
  }) }) });
}

export function lispLambdaExpr(params: ReadonlyArray<string>): ((x: LispSyntax.Expression) => LispSyntax.Expression) {
  return ((body: LispSyntax.Expression) => ({ tag: "lambda", value: ({
    name: null,
    params: LibLists.map(((p: string) => p))(params),
    restParam: null,
    body: [body]
  }) }));
}

export function lispListExpr(elements: ReadonlyArray<LispSyntax.Expression>): LispSyntax.Expression {
  return ({ tag: "list", value: ({
    elements: elements,
    quoted: false
  }) });
}

export function lispLitExpr(lit: LispSyntax.Literal): LispSyntax.Expression {
  return ({ tag: "literal", value: lit });
}

export function lispNamedLambdaExpr(name: string): ((x: ReadonlyArray<string>) => ((x: LispSyntax.Expression) => LispSyntax.Expression)) {
  return ((params: ReadonlyArray<string>) => ((body: LispSyntax.Expression) => ({ tag: "lambda", value: ({
    name: name,
    params: LibLists.map(((p: string) => p))(params),
    restParam: null,
    body: [body]
  }) })));
}

export const lispNilExpr: LispSyntax.Expression = ({ tag: "literal", value: ({ tag: "nil" }) });

export function lispSymbol(name: string): LispSyntax.Symbol {
  return name;
}

export function lispTopForm(form: LispSyntax.TopLevelForm): LispSyntax.TopLevelFormWithComments {
  return ({
    doc: null,
    comment: null,
    form: form
  });
}

export function lispTopFormWithComments(mdoc: string | null): ((x: LispSyntax.TopLevelForm) => LispSyntax.TopLevelFormWithComments) {
  return ((form: LispSyntax.TopLevelForm) => ({
    doc: LibMaybes.map(((d: string) => d))(mdoc),
    comment: null,
    form: form
  }));
}

export function lispVar(name: string): LispSyntax.Expression {
  return ({ tag: "variable", value: ({
    name: name,
    functionNamespace: false
  }) });
}

export function moduleExports(forms: ReadonlyArray<LispSyntax.TopLevelFormWithComments>): ReadonlyArray<LispSyntax.ExportDeclaration> {
  return (() => {
  const symbols = LibLists.concat(LibLists.map(((fwc: LispSyntax.TopLevelFormWithComments) => (() => {
  const form = ((_x) => _x.form)(fwc);
  return (() => {
  const _m = form;
  switch (_m.tag) {
    case "variable": return ((vd: LispSyntax.VariableDefinition) => [((_x) => _x.name)(vd)])((_m as any).value);
    case "recordType": return ((rdef: LispSyntax.RecordTypeDefinition) => (() => {
  const rname = ((_x) => _x)(((_x) => _x.name)(rdef));
  return (() => {
  const fields = ((_x) => _x.fields)(rdef);
  return (() => {
  const fieldSyms = LibLists.map(((f: LispSyntax.FieldDefinition) => (() => {
  const fn = ((_x) => _x)(((_x) => _x.name)(f));
  return LibStrings.cat([rname, "-", fn]);
})()))(fields);
  return LibLists.concat([[LibStrings.cat2("make-")(rname), LibStrings.cat2(rname)("?")], fieldSyms]);
})();
})();
})())((_m as any).value);
    default: return [](_m);
  }
})();
})()))(forms));
  return LibLogic.ifElse(LibLists.null_(symbols))([])([({
    symbols: symbols
  })]);
})();
}

export function moduleImports(focusNs: Packaging.Namespace): ((x: ReadonlyArray<Packaging.Definition>) => ReadonlyArray<LispSyntax.ImportDeclaration>) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => (() => {
  const depNss = LibSets.toList(LibSets.delete_(focusNs)(Analysis.definitionDependencyNamespaces(defs)));
  return LibLists.map(((ns: Packaging.Namespace) => ({
    module: ((_x) => _x)(ns),
    spec: ({ tag: "all" })
  })))(depNss);
})());
}

export function moduleToLisp<t0, t1, t2>(dialect: LispSyntax.Dialect): ((x: Packaging.Module) => ((x: ReadonlyArray<Packaging.Definition>) => ((x: t0) => ((x: t1) => t2 | LispSyntax.Program)))) {
  return ((mod: Packaging.Module) => ((defs0: ReadonlyArray<Packaging.Definition>) => ((cx: t0) => ((g: t1) => (() => {
  const defs = Environment.reorderDefs(defs0);
  return (() => {
  const partitioned = Environment.partitionDefinitions(defs);
  return (() => {
  const allTypeDefs = LibPairs.first(partitioned);
  return (() => {
  const termDefs = LibPairs.second(partitioned);
  return (() => {
  const typeDefs = LibLists.filter(((td: Packaging.TypeDefinition) => Predicates.isNominalType(((_x) => _x.type)(((_x) => _x.type)(td)))))(allTypeDefs);
  return LibEithers.bind(LibEithers.mapList(((v1: Packaging.TypeDefinition) => encodeTypeDefinition(cx)(g)(v1)))(typeDefs))(((typeItems: ReadonlyArray<LispSyntax.TopLevelFormWithComments>) => LibEithers.bind(LibEithers.mapList(((v1: Packaging.TermDefinition) => encodeTermDefinition(dialect)(cx)(g)(v1)))(termDefs))(((termItems: ReadonlyArray<LispSyntax.TopLevelFormWithComments>) => (() => {
  const allItems = LibLists.concat2(typeItems)(termItems);
  return (() => {
  const nsName = ((_x) => _x)(((_x) => _x.namespace)(mod));
  return (() => {
  const focusNs = ((_x) => _x.namespace)(mod);
  return (() => {
  const imports = moduleImports(focusNs)(defs);
  return (() => {
  const exports = moduleExports(allItems);
  return ({ tag: "right", value: ({
    dialect: dialect,
    module: ({
    name: nsName,
    doc: null
  }),
    imports: imports,
    exports: exports,
    forms: allItems
  }) });
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})()))));
}

export function qualifiedSnakeName(name: Core.Name): string {
  return (() => {
  const raw = ((_x) => _x)(name);
  return (() => {
  const parts = LibStrings.splitOn(".")(raw);
  return (() => {
  const snakeParts = LibLists.map(((p: string) => Formatting.convertCaseCamelOrUnderscoreToLowerSnake(p)))(parts);
  return (() => {
  const joined = LibStrings.intercalate("_")(snakeParts);
  return Formatting.sanitizeWithUnderscores(LispLanguage.lispReservedWords)(joined);
})();
})();
})();
})();
}

export function qualifiedTypeName(name: Core.Name): string {
  return Formatting.capitalize(Names.localNameOf(name));
}

export function wrapInThunk(expr: LispSyntax.Expression): LispSyntax.Expression {
  return ({ tag: "lambda", value: ({
    name: null,
    params: [],
    restParam: null,
    body: [expr]
  }) });
}
