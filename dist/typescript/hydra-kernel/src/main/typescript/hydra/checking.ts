// Note: this is an automatically generated file. Do not edit.

/**
 * Type checking and type reconstruction (type-of) for the results of Hydra unification and inference
 */



import * as Ast from "./ast.js";
import * as Classes from "./classes.js";
import * as Coders from "./coders.js";
import * as Constants from "./constants.js";
import * as Context from "./context.js";
import * as Core from "./core.js";
import * as Dependencies from "./dependencies.js";
import * as ErrorChecking from "./error/checking.js";
import * as ErrorCore from "./error/core.js";
import * as ErrorPackaging from "./error/packaging.js";
import * as Errors from "./errors.js";
import * as ExtractCore from "./extract/core.js";
import * as Formatting from "./formatting.js";
import * as Graph from "./graph.js";
import * as JsonModel from "./json/model.js";
import * as Lexical from "./lexical.js";
import * as LibEithers from "./lib/eithers.js";
import * as LibEquality from "./lib/equality.js";
import * as LibLists from "./lib/lists.js";
import * as LibLiterals from "./lib/literals.js";
import * as LibLogic from "./lib/logic.js";
import * as LibMaps from "./lib/maps.js";
import * as LibMaybes from "./lib/maybes.js";
import * as LibPairs from "./lib/pairs.js";
import * as LibSets from "./lib/sets.js";
import * as LibStrings from "./lib/strings.js";
import * as Names from "./names.js";
import * as Packaging from "./packaging.js";
import * as Parsing from "./parsing.js";
import * as Paths from "./paths.js";
import * as Phantoms from "./phantoms.js";
import * as Query from "./query.js";
import * as Reflect from "./reflect.js";
import * as Relational from "./relational.js";
import * as Resolution from "./resolution.js";
import * as Rewriting from "./rewriting.js";
import * as Scoping from "./scoping.js";
import * as ShowCore from "./show/core.js";
import * as ShowErrors from "./show/errors.js";
import * as ShowVariants from "./show/variants.js";
import * as Strip from "./strip.js";
import * as Substitution from "./substitution.js";
import * as Tabular from "./tabular.js";
import * as Testing from "./testing.js";
import * as Topology from "./topology.js";
import * as Typing from "./typing.js";
import * as Util from "./util.js";
import * as Variables from "./variables.js";
import * as Variants from "./variants.js";

export function allEqual<t0>(els: ReadonlyArray<t0>): boolean {
  return LibLogic.ifElse(LibLists.null_(els))(true)(LibLists.foldl(((b: boolean) => ((t: t0) => LibLogic.and(b)(LibEquality.equal(t)(LibLists.head(els))))))(true)(LibLists.tail(els)));
}

export function applyTypeArgumentsToType<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Type) => Errors.Error | Core.Type))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((t: Core.Type) => LibLogic.ifElse(LibLists.null_(typeArgs))(({ tag: "right", value: t }))((() => {
  const nonnull = (() => {
  const _m = t;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(ft);
  return (() => {
  const tbody = ((_x) => _x.body)(ft);
  return applyTypeArgumentsToType(cx)(tx)(LibLists.tail(typeArgs))(Substitution.substInType(LibMaps.singleton(v)(LibLists.head(typeArgs)))(tbody));
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "extraction", value: ({ tag: "unexpectedShape", value: ({
    expected: "forall type",
    actual: LibStrings.cat([ShowCore.type(t), ". Trying to apply ", LibLiterals.showInt32(LibLists.length(typeArgs)), " type args: ", Formatting.showList(ShowCore.type)(typeArgs), ". Context has vars: {", LibStrings.intercalate(", ")(LibLists.map(((_x) => _x))(LibMaps.keys(((_x) => _x.boundTypes)(tx)))), "}"])
  }) }) }) })(_m);
  }
})();
  return nonnull;
})()))));
}

export function checkForUnboundTypeVariables<t0>(cx: t0): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | void)) {
  return ((tx: Graph.Graph) => ((term0: Core.Term) => (() => {
  const svars = LibSets.fromList(LibMaps.keys(((_x) => _x.schemaTypes)(tx)));
  return (() => {
  const checkRecursive = ((vars: ReadonlySet<Core.Name>) => ((trace: ReadonlyArray<string>) => ((lbinding: Core.Binding | null) => ((term: Core.Term) => (() => {
  const recurse = ((v1: Core.Term) => checkRecursive(vars)(trace)(lbinding)(v1));
  return (() => {
  const dflt = LibEithers.bind(LibEithers.mapList(recurse)(Rewriting.subterms(term)))(((_: ReadonlyArray<void>) => ({ tag: "right", value: undefined })));
  return (() => {
  const check = ((typ: Core.Type) => (() => {
  const freevars = Variables.freeVariablesInType(typ);
  return (() => {
  const badvars = LibSets.difference(LibSets.difference(freevars)(vars))(svars);
  return LibLogic.ifElse(LibSets.null_(badvars))(({ tag: "right", value: undefined }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "unboundTypeVariables", value: ({
    variables: badvars,
    type: typ
  }) }) }) }));
})();
})());
  return (() => {
  const checkOptional = ((m: Core.Type | null) => LibEithers.bind(LibEithers.mapMaybe(check)(m))(((_: void | null) => ({ tag: "right", value: undefined }))));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => LibEithers.bind(checkOptional(((_x) => _x.domain)(l)))(((_: void) => recurse(((_x) => _x.body)(l)))))((_m as any).value);
    case "let": return ((l: Core.Let) => (() => {
  const forBinding = ((b: Core.Binding) => (() => {
  const bterm = ((_x) => _x.term)(b);
  return (() => {
  const newVars = LibMaybes.maybe(vars)(((ts: Core.TypeScheme) => LibSets.union(vars)(LibSets.fromList(((_x) => _x.variables)(ts)))))(((_x) => _x.type)(b));
  return (() => {
  const newTrace = LibLists.cons(((_x) => _x)(((_x) => _x.name)(b)))(trace);
  return checkRecursive(newVars)(newTrace)(b)(bterm);
})();
})();
})());
  return LibEithers.bind(LibEithers.mapList(forBinding)(((_x) => _x.bindings)(l)))(((_: ReadonlyArray<void>) => recurse(((_x) => _x.body)(l))));
})())((_m as any).value);
    case "typeApplication": return ((tt: Core.TypeApplicationTerm) => LibEithers.bind(check(((_x) => _x.type)(tt)))(((_: void) => recurse(((_x) => _x.body)(tt)))))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => LibEithers.bind(check(({ tag: "variable", value: ((_x) => _x.parameter)(tl) })))(((_: void) => recurse(((_x) => _x.body)(tl)))))((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})();
})();
})()))));
  return checkRecursive(LibSets.empty)(["top level"])(null)(term0);
})();
})()));
}

export function checkNominalApplication(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Name) => ((x: ReadonlyArray<Core.Type>) => Errors.Error | readonly [void, Context.Context]))) {
  return ((tx: Graph.Graph) => ((tname: Core.Name) => ((typeArgs: ReadonlyArray<Core.Type>) => LibEithers.bind(Resolution.requireSchemaType(cx)(((_x) => _x.schemaTypes)(tx))(tname))(((result: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(result);
  return (() => {
  const cx2 = LibPairs.second(result);
  return (() => {
  const vars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const varslen = LibLists.length(vars);
  return (() => {
  const argslen = LibLists.length(typeArgs);
  return LibLogic.ifElse(LibEquality.equal(varslen)(argslen))(({ tag: "right", value: [undefined, cx2] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "variable", value: tname }),
    expectedArity: varslen,
    actualArity: argslen,
    typeArguments: typeArgs
  }) }) }) }));
})();
})();
})();
})();
})())))));
}

export function checkSameType<t0>(cx: t0): ((x: Graph.Graph) => ((x: string) => ((x: ReadonlyArray<Core.Type>) => Errors.Error | Core.Type))) {
  return ((tx: Graph.Graph) => ((desc: string) => ((types: ReadonlyArray<Core.Type>) => LibLogic.ifElse(typesAllEffectivelyEqual(tx)(types))(({ tag: "right", value: LibLists.head(types) }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "unequalTypes", value: ({
    types: types,
    description: desc
  }) }) }) })))));
}

export function checkType(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => ((x: Core.Type) => Errors.Error | void))) {
  return ((tx: Graph.Graph) => ((term: Core.Term) => ((typ: Core.Type) => (() => {
  const vars = ((_x) => _x.typeVariables)(tx);
  return LibLogic.ifElse(Constants.debugInference)(LibEithers.bind(LibEithers.map(((_p: readonly [Core.Type, Context.Context]) => LibPairs.first(_p)))(typeOf(cx)(tx)([])(term)))(((t0: Core.Type) => LibLogic.ifElse(typesEffectivelyEqual(tx)(t0)(typ))(({ tag: "right", value: undefined }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeMismatch", value: ({
    expectedType: typ,
    actualType: t0
  }) }) }) })))))(({ tag: "right", value: undefined }));
})())));
}

export function checkTypeSubst<t0>(cx: t0): ((x: Graph.Graph) => ((x: Typing.TypeSubst) => Errors.Error | Typing.TypeSubst)) {
  return ((tx: Graph.Graph) => ((subst: Typing.TypeSubst) => (() => {
  const s = ((_x) => _x)(subst);
  return (() => {
  const vars = LibSets.fromList(LibMaps.keys(s));
  return (() => {
  const suspectVars = LibSets.intersection(vars)(LibSets.fromList(LibMaps.keys(((_x) => _x.schemaTypes)(tx))));
  return (() => {
  const isNominal = ((ts: Core.TypeScheme) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.type)(ts));
  switch (_m.tag) {
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => true)((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => true)((_m as any).value);
    case "wrap": return ((_: Core.Type) => true)((_m as any).value);
    default: return false(_m);
  }
})());
  return (() => {
  const badVars = LibSets.fromList(LibLists.filter(((v: Core.Name) => LibMaybes.maybe(false)(isNominal)(Lexical.dereferenceSchemaType(v)(((_x) => _x.schemaTypes)(tx)))))(LibSets.toList(suspectVars)));
  return (() => {
  const badPairs = LibLists.filter(((p: readonly [Core.Name, Core.Type]) => LibSets.member(LibPairs.first(p))(badVars)))(LibMaps.toList(s));
  return (() => {
  const printPair = ((p: readonly [Core.Name, Core.Type]) => LibStrings.cat2(LibStrings.cat2(((_x) => _x)(LibPairs.first(p)))(" --> "))(ShowCore.type(LibPairs.second(p))));
  return LibLogic.ifElse(LibSets.null_(badVars))(({ tag: "right", value: subst }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "incorrectUnification", value: ({
    substitution: subst
  }) }) }) }));
})();
})();
})();
})();
})();
})();
})()));
}

export function checkTypeVariables<t0, t1>(_tx: t0): ((x: t1) => void) {
  return ((_typ: t1) => undefined);
}

export function containsInScopeTypeVars(tx: Graph.Graph): ((x: Core.Type) => boolean) {
  return ((t: Core.Type) => (() => {
  const vars = ((_x) => _x.typeVariables)(tx);
  return (() => {
  const freeVars = Variables.freeVariablesInTypeSimple(t);
  return LibLogic.not(LibSets.null_(LibSets.intersection(vars)(freeVars)));
})();
})());
}

export function normalizeTypeFreeVars(typ: Core.Type): Core.Type {
  return (() => {
  const collectVars = ((acc: ReadonlyMap<Core.Name, Core.Name>) => ((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibMaps.member(v)(acc))(acc)(LibMaps.insert(v)(LibStrings.cat2("_tv")(LibLiterals.showInt32(LibMaps.size(acc))))(acc)))((_m as any).value);
    default: return acc(_m);
  }
})()));
  return (() => {
  const subst = Rewriting.foldOverType(({ tag: "pre" }))(collectVars)(LibMaps.empty)(typ);
  return Variables.substituteTypeVariables(subst)(typ);
})();
})();
}

export function toFContext(cx: Graph.Graph): ReadonlyMap<Core.Name, Core.Type> {
  return LibMaps.map(Scoping.typeSchemeToFType)(((_x) => _x.boundTypes)(cx));
}

export function typeListsEffectivelyEqual(tx: Graph.Graph): ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Type>) => boolean)) {
  return ((tlist1: ReadonlyArray<Core.Type>) => ((tlist2: ReadonlyArray<Core.Type>) => LibLogic.ifElse(LibEquality.equal(LibLists.length(tlist1))(LibLists.length(tlist2)))(LibLists.foldl(LibLogic.and)(true)(LibLists.zipWith(((v1: Core.Type) => ((v2: Core.Type) => typesEffectivelyEqual(tx)(v1)(v2))))(tlist1)(tlist2)))(false)));
}

export function typeOf(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((term: Core.Term) => (() => {
  const cx1 = ({
    trace: LibLists.cons("typeOf")(((_x) => _x.trace)(cx)),
    messages: ((_x) => _x.messages)(cx),
    other: ((_x) => _x.other)(cx)
  });
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((v1: Core.AnnotatedTerm) => typeOfAnnotatedTerm(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "application": return ((v1: Core.Application) => typeOfApplication(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "cases": return ((v1: Core.CaseStatement) => typeOfCaseStatement(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "either": return ((v1: Core.Term | Core.Term) => typeOfEither(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "lambda": return ((v1: Core.Lambda) => typeOfLambda(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "let": return ((v1: Core.Let) => typeOfLet(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "list": return ((v1: ReadonlyArray<Core.Term>) => typeOfList(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "literal": return ((v1: Core.Literal) => typeOfLiteral(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "map": return ((v1: ReadonlyMap<Core.Term, Core.Term>) => typeOfMap(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "maybe": return ((v1: Core.Term | null) => typeOfMaybe(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "pair": return ((v1: readonly [Core.Term, Core.Term]) => typeOfPair(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "project": return ((v1: Core.Projection) => typeOfProjection(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "record": return ((v1: Core.Record) => typeOfRecord(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "set": return ((v1: ReadonlySet<Core.Term>) => typeOfSet(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "typeApplication": return ((v1: Core.TypeApplicationTerm) => typeOfTypeApplication(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "typeLambda": return ((v1: Core.TypeLambda) => typeOfTypeLambda(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "inject": return ((v1: Core.Injection) => typeOfInjection(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "unit": return ((_: void) => typeOfUnit(cx1)(tx)(typeArgs))((_m as any).value);
    case "unwrap": return ((v1: Core.Name) => typeOfUnwrap(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "variable": return ((v1: Core.Name) => typeOfVariable(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    case "wrap": return ((v1: Core.WrappedTerm) => typeOfWrappedTerm(cx1)(tx)(typeArgs)(v1))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "checking", value: ({ tag: "unsupportedTermVariant", value: ({
    termVariant: Reflect.termVariant(term)
  }) }) }) })(_m);
  }
})();
})())));
}

export function typeOfAnnotatedTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.AnnotatedTerm) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((at: Core.AnnotatedTerm) => typeOf(cx)(tx)(typeArgs)(((_x) => _x.body)(at)))));
}

export function typeOfApplication(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Application) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  return (() => {
  const arg = ((_x) => _x.argument)(app);
  return (() => {
  const tryType = ((cx0: Context.Context) => ((tfun: Core.Type) => ((targ: Core.Type) => (() => {
  const _m = tfun;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => tryType(cx0)(((_x) => _x.body)(ft))(targ))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(ft);
  return (() => {
  const cod = ((_x) => _x.codomain)(ft);
  return LibLogic.ifElse(typesEffectivelyEqual(tx)(dom)(targ))(({ tag: "right", value: [cod, cx0] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeMismatch", value: ({
    expectedType: dom,
    actualType: targ
  }) }) }) }));
})();
})())((_m as any).value);
    case "variable": return ((v: Core.Name) => (() => {
  const nameResult = Names.freshName(cx0);
  return (() => {
  const freshN = LibPairs.first(nameResult);
  return (() => {
  const cx1 = LibPairs.second(nameResult);
  return ({ tag: "right", value: [({ tag: "variable", value: freshN }), cx1] });
})();
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "checking", value: ({ tag: "notAFunctionType", value: ({
    type: tfun
  }) }) }) })(_m);
  }
})())));
  return LibEithers.bind(typeOf(cx)(tx)([])(fun))(((result1: readonly [Core.Type, Context.Context]) => (() => {
  const tfun = LibPairs.first(result1);
  return (() => {
  const cx2 = LibPairs.second(result1);
  return LibEithers.bind(typeOf(cx2)(tx)([])(arg))(((result2: readonly [Core.Type, Context.Context]) => (() => {
  const targ = LibPairs.first(result2);
  return (() => {
  const cx3 = LibPairs.second(result2);
  return LibEithers.bind(tryType(cx3)(tfun)(targ))(((result3: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(result3);
  return (() => {
  const cx4 = LibPairs.second(result3);
  return LibEithers.bind(applyTypeArgumentsToType(cx4)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx4] })));
})();
})()));
})();
})()));
})();
})()));
})();
})();
})())));
}

export function typeOfCaseStatement(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.CaseStatement) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases = ((_x) => _x.cases)(cs);
  return (() => {
  const cterms = LibLists.map(((_x) => _x.term))(cases);
  return LibEithers.bind(LibEithers.mapMaybe(((e: Core.Term) => typeOf(cx)(tx)([])(e)))(dflt))(((dfltResult: readonly [Core.Type, Context.Context] | null) => (() => {
  const tdflt = LibMaybes.map(LibPairs.first)(dfltResult);
  return (() => {
  const cx2 = LibMaybes.maybe(cx)(LibPairs.second)(dfltResult);
  return (() => {
  const foldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((term: Core.Term) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(term))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx2] }))(cterms);
  return LibEithers.bind(foldResult)(((foldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const tcterms = LibPairs.first(foldR);
  return (() => {
  const cx3 = LibPairs.second(foldR);
  return (() => {
  const fcodsResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((t: Core.Type) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const cods = LibPairs.first(accR);
  return LibEithers.bind(ExtractCore.functionType(t))(((ft: Core.FunctionType) => ({ tag: "right", value: [LibLists.concat2(cods)(LibLists.pure(((_x) => _x.codomain)(ft))), cx3] })));
})())))))(({ tag: "right", value: [[], cx3] }))(tcterms);
  return LibEithers.bind(fcodsResult)(((fcodsR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const fcods = LibPairs.first(fcodsR);
  return (() => {
  const cods = LibMaybes.cat(LibLists.cons(tdflt)(LibLists.map(LibMaybes.pure)(fcods)));
  return LibEithers.bind(checkSameType(cx3)(tx)("case branches")(cods))(((cod: Core.Type) => ({ tag: "right", value: [({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(typeArgs),
    codomain: cod
  }) }), cx3] })));
})();
})()));
})();
})();
})()));
})();
})();
})()));
})();
})();
})();
})())));
}

export function typeOfEither(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term | Core.Term) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((et: Core.Term | Core.Term) => (() => {
  const n = LibLists.length(typeArgs);
  return LibLogic.ifElse(LibEquality.equal(n)(2))(LibEithers.either(((leftTerm: Core.Term) => LibEithers.bind(typeOf(cx)(tx)([])(leftTerm))(((result: readonly [Core.Type, Context.Context]) => (() => {
  const leftType = LibPairs.first(result);
  return (() => {
  const cx2 = LibPairs.second(result);
  return ({ tag: "right", value: [({ tag: "either", value: ({
    left: leftType,
    right: LibLists.at(1)(typeArgs)
  }) }), cx2] });
})();
})()))))(((rightTerm: Core.Term) => LibEithers.bind(typeOf(cx)(tx)([])(rightTerm))(((result: readonly [Core.Type, Context.Context]) => (() => {
  const rightType = LibPairs.first(result);
  return (() => {
  const cx2 = LibPairs.second(result);
  return ({ tag: "right", value: [({ tag: "either", value: ({
    left: LibLists.at(0)(typeArgs),
    right: rightType
  }) }), cx2] });
})();
})()))))(et))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "either", value: ({
    left: ({ tag: "unit" }),
    right: ({ tag: "unit" })
  }) }),
    expectedArity: 2,
    actualArity: n,
    typeArguments: typeArgs
  }) }) }) }));
})())));
}

export function typeOfInjection(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Injection) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((injection: Core.Injection) => (() => {
  const tname = ((_x) => _x.typeName)(injection);
  return (() => {
  const field = ((_x) => _x.field)(injection);
  return (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return LibEithers.bind(Resolution.requireSchemaType(cx)(((_x) => _x.schemaTypes)(tx))(tname))(((schemaResult: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(schemaResult);
  return (() => {
  const cx2 = LibPairs.second(schemaResult);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const sbody = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.unionType(tname)(sbody))(((sfields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(Resolution.findFieldType(cx2)(fname)(sfields))(((ftyp: Core.Type) => ({ tag: "right", value: [Resolution.nominalApplication(tname)(typeArgs), cx2] })))));
})();
})();
})();
})()));
})();
})();
})();
})())));
}

export function typeOfLambda(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Lambda) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((l: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(l);
  return (() => {
  const mdom = ((_x) => _x.domain)(l);
  return (() => {
  const body = ((_x) => _x.body)(l);
  return LibEithers.bind(LibMaybes.maybe(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "untypedLambda", value: ({

  }) }) }) }))(((dom: Core.Type) => (() => {
  const types2 = LibMaps.insert(v)(Scoping.fTypeToTypeScheme(dom))(((_x) => _x.boundTypes)(tx));
  return LibEithers.bind(typeOf(cx)(({
    boundTerms: ((_x) => _x.boundTerms)(tx),
    boundTypes: types2,
    classConstraints: ((_x) => _x.classConstraints)(tx),
    lambdaVariables: ((_x) => _x.lambdaVariables)(tx),
    metadata: ((_x) => _x.metadata)(tx),
    primitives: ((_x) => _x.primitives)(tx),
    schemaTypes: ((_x) => _x.schemaTypes)(tx),
    typeVariables: ((_x) => _x.typeVariables)(tx)
  }))([])(body))(((codResult: readonly [Core.Type, Context.Context]) => (() => {
  const cod = LibPairs.first(codResult);
  return (() => {
  const cx2 = LibPairs.second(codResult);
  return ({ tag: "right", value: [({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }), cx2] });
})();
})()));
})()))(mdom))(((tbodyResult: readonly [Core.Type, Context.Context]) => (() => {
  const tbody = LibPairs.first(tbodyResult);
  return (() => {
  const cx3 = LibPairs.second(tbodyResult);
  return LibEithers.bind(applyTypeArgumentsToType(cx3)(tx)(typeArgs)(tbody))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx3] })));
})();
})()));
})();
})();
})())));
}

export function typeOfLet(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Let) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((letTerm: Core.Let) => (() => {
  const bs = ((_x) => _x.bindings)(letTerm);
  return (() => {
  const body = ((_x) => _x.body)(letTerm);
  return (() => {
  const bnames = LibLists.map(((_x) => _x.name))(bs);
  return (() => {
  const bindingType = ((b: Core.Binding) => LibMaybes.maybe(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "untypedLetBinding", value: ({
    binding: b
  }) }) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: Scoping.typeSchemeToFType(ts) })))(((_x) => _x.type)(b)));
  return (() => {
  const btypesResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, void]) => ((b: Core.Binding) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, void]) => (() => {
  const types = LibPairs.first(accR);
  return LibEithers.bind(bindingType(b))(((btype: Core.Type) => ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(btype)), undefined] })));
})())))))(({ tag: "right", value: [[], undefined] }))(bs);
  return LibEithers.bind(btypesResult)(((btypesR: readonly [ReadonlyArray<Core.Type>, void]) => (() => {
  const btypes = LibPairs.first(btypesR);
  return (() => {
  const tx2 = ({
    boundTerms: ((_x) => _x.boundTerms)(tx),
    boundTypes: LibMaps.union(LibMaps.fromList(LibLists.zip(bnames)(LibLists.map(Scoping.fTypeToTypeScheme)(btypes))))(((_x) => _x.boundTypes)(tx)),
    classConstraints: ((_x) => _x.classConstraints)(tx),
    lambdaVariables: ((_x) => _x.lambdaVariables)(tx),
    metadata: ((_x) => _x.metadata)(tx),
    primitives: ((_x) => _x.primitives)(tx),
    schemaTypes: ((_x) => _x.schemaTypes)(tx),
    typeVariables: ((_x) => _x.typeVariables)(tx)
  });
  return LibEithers.bind(typeOf(cx)(tx2)([])(body))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cx2 = LibPairs.second(tResult);
  return LibEithers.bind(applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx2] })));
})();
})()));
})();
})()));
})();
})();
})();
})();
})())));
}

export function typeOfList(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Term>) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((els: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(els))(LibLogic.ifElse(LibEquality.equal(LibLists.length(typeArgs))(1))(({ tag: "right", value: [({ tag: "list", value: LibLists.head(typeArgs) }), cx] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "list", value: ({ tag: "unit" }) }),
    expectedArity: 1,
    actualArity: LibLists.length(typeArgs),
    typeArguments: typeArgs
  }) }) }) })))((() => {
  const foldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((term: Core.Term) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(term))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx] }))(els);
  return LibEithers.bind(foldResult)(((foldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const eltypes = LibPairs.first(foldR);
  return (() => {
  const cx2 = LibPairs.second(foldR);
  return LibEithers.bind(checkSameType(cx2)(tx)("list elements")(eltypes))(((unifiedType: Core.Type) => ({ tag: "right", value: [({ tag: "list", value: unifiedType }), cx2] })));
})();
})()));
})()))));
}

export function typeOfLiteral<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Literal) => Errors.Error | readonly [Core.Type, t0]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((lit: Core.Literal) => (() => {
  const t = ({ tag: "literal", value: Reflect.literalType(lit) });
  return LibEithers.bind(applyTypeArgumentsToType(cx)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx] })));
})())));
}

export function typeOfMap(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyMap<Core.Term, Core.Term>) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLogic.ifElse(LibMaps.null_(m))(LibLogic.ifElse(LibEquality.equal(LibLists.length(typeArgs))(2))(({ tag: "right", value: [({ tag: "map", value: ({
    keys: LibLists.at(0)(typeArgs),
    values: LibLists.at(1)(typeArgs)
  }) }), cx] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "map", value: ({
    keys: ({ tag: "unit" }),
    values: ({ tag: "unit" })
  }) }),
    expectedArity: 2,
    actualArity: LibLists.length(typeArgs),
    typeArguments: typeArgs
  }) }) }) })))((() => {
  const pairs = LibMaps.toList(m);
  return (() => {
  const keyFoldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(LibPairs.first(p)))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx] }))(pairs);
  return LibEithers.bind(keyFoldResult)(((keyFoldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const keyTypes = LibPairs.first(keyFoldR);
  return (() => {
  const cx2 = LibPairs.second(keyFoldR);
  return LibEithers.bind(checkSameType(cx2)(tx)("map keys")(keyTypes))(((kt: Core.Type) => (() => {
  const valFoldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(LibPairs.second(p)))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx2] }))(pairs);
  return LibEithers.bind(valFoldResult)(((valFoldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const valTypes = LibPairs.first(valFoldR);
  return (() => {
  const cx3 = LibPairs.second(valFoldR);
  return LibEithers.bind(checkSameType(cx3)(tx)("map values")(valTypes))(((vt: Core.Type) => LibEithers.bind(applyTypeArgumentsToType(cx3)(tx)(typeArgs)(({ tag: "map", value: ({
    keys: kt,
    values: vt
  }) })))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx3] })))));
})();
})()));
})()));
})();
})()));
})();
})()))));
}

export function typeOfMaybe(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term | null) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((mt: Core.Term | null) => (() => {
  const forNothing = (() => {
  const n = LibLists.length(typeArgs);
  return LibLogic.ifElse(LibEquality.equal(n)(1))(({ tag: "right", value: [({ tag: "maybe", value: LibLists.head(typeArgs) }), cx] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "maybe", value: ({ tag: "unit" }) }),
    expectedArity: 1,
    actualArity: n,
    typeArguments: typeArgs
  }) }) }) }));
})();
  return (() => {
  const forJust = ((term: Core.Term) => LibEithers.bind(typeOf(cx)(tx)([])(term))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const termType = LibPairs.first(tResult);
  return (() => {
  const cx2 = LibPairs.second(tResult);
  return (() => {
  const t = ({ tag: "maybe", value: termType });
  return LibEithers.bind(applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx2] })));
})();
})();
})())));
  return LibMaybes.maybe(forNothing)(forJust)(mt);
})();
})())));
}

export function typeOfPair(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: readonly [Core.Term, Core.Term]) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((p: readonly [Core.Term, Core.Term]) => (() => {
  const n = LibLists.length(typeArgs);
  return LibLogic.ifElse(LibEquality.equal(n)(2))((() => {
  const pairFst = LibPairs.first(p);
  return (() => {
  const pairSnd = LibPairs.second(p);
  return LibEithers.bind(typeOf(cx)(tx)([])(pairFst))(((result1: readonly [Core.Type, Context.Context]) => (() => {
  const firstType = LibPairs.first(result1);
  return (() => {
  const cx2 = LibPairs.second(result1);
  return LibEithers.bind(typeOf(cx2)(tx)([])(pairSnd))(((result2: readonly [Core.Type, Context.Context]) => (() => {
  const secondType = LibPairs.first(result2);
  return (() => {
  const cx3 = LibPairs.second(result2);
  return ({ tag: "right", value: [({ tag: "pair", value: ({
    first: firstType,
    second: secondType
  }) }), cx3] });
})();
})()));
})();
})()));
})();
})())(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "pair", value: ({
    first: ({ tag: "unit" }),
    second: ({ tag: "unit" })
  }) }),
    expectedArity: 2,
    actualArity: n,
    typeArguments: typeArgs
  }) }) }) }));
})())));
}

export function typeOfPrimitive(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Name) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((name: Core.Name) => (() => {
  const rawTs = LibMaybes.map(((_p: Graph.Primitive) => ((_x) => _x.type)(_p)))(LibMaps.lookup(name)(((_x) => _x.primitives)(tx)));
  return LibMaybes.maybe(({ tag: "left", value: ({ tag: "undefinedTermVariable", value: ({
    location: [],
    name: name
  }) }) }))(((tsRaw: Core.TypeScheme) => (() => {
  const instResult = Resolution.instantiateTypeScheme(cx)(tsRaw);
  return (() => {
  const ts = LibPairs.first(instResult);
  return (() => {
  const cx2 = LibPairs.second(instResult);
  return (() => {
  const t = Scoping.typeSchemeToFType(ts);
  return LibEithers.bind(applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx2] })));
})();
})();
})();
})()))(rawTs);
})())));
}

export function typeOfProjection(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Projection) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((p: Core.Projection) => (() => {
  const tname = ((_x) => _x.typeName)(p);
  return (() => {
  const fname = ((_x) => _x.field)(p);
  return LibEithers.bind(Resolution.requireSchemaType(cx)(((_x) => _x.schemaTypes)(tx))(tname))(((schemaResult: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(schemaResult);
  return (() => {
  const cx2 = LibPairs.second(schemaResult);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const sbody = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.recordType(tname)(sbody))(((sfields: ReadonlyArray<Core.FieldType>) => LibEithers.bind(Resolution.findFieldType(cx2)(fname)(sfields))(((ftyp: Core.Type) => (() => {
  const subst = LibMaps.fromList(LibLists.zip(svars)(typeArgs));
  return (() => {
  const sftyp = Substitution.substInType(subst)(ftyp);
  return ({ tag: "right", value: [({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(typeArgs),
    codomain: sftyp
  }) }), cx2] });
})();
})()))));
})();
})();
})();
})()));
})();
})())));
}

export function typeOfRecord(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Record) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((record: Core.Record) => (() => {
  const tname = ((_x) => _x.typeName)(record);
  return (() => {
  const fields = ((_x) => _x.fields)(record);
  return (() => {
  const foldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((term: Core.Term) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(term))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx] }))(LibLists.map(((_x) => _x.term))(fields));
  return LibEithers.bind(foldResult)(((foldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const cx2 = LibPairs.second(foldR);
  return ({ tag: "right", value: [Resolution.nominalApplication(tname)(typeArgs), cx2] });
})()));
})();
})();
})())));
}

export function typeOfSet(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlySet<Core.Term>) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((els: ReadonlySet<Core.Term>) => LibLogic.ifElse(LibSets.null_(els))(LibLogic.ifElse(LibEquality.equal(LibLists.length(typeArgs))(1))(({ tag: "right", value: [({ tag: "set", value: LibLists.head(typeArgs) }), cx] }))(({ tag: "left", value: ({ tag: "checking", value: ({ tag: "typeArityMismatch", value: ({
    type: ({ tag: "set", value: ({ tag: "unit" }) }),
    expectedArity: 1,
    actualArity: LibLists.length(typeArgs),
    typeArguments: typeArgs
  }) }) }) })))((() => {
  const foldResult = LibLists.foldl(((acc: Errors.Error | readonly [ReadonlyArray<Core.Type>, Context.Context]) => ((term: Core.Term) => LibEithers.bind(acc)(((accR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const types = LibPairs.first(accR);
  return (() => {
  const cxA = LibPairs.second(accR);
  return LibEithers.bind(typeOf(cxA)(tx)([])(term))(((tResult: readonly [Core.Type, Context.Context]) => (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cxB = LibPairs.second(tResult);
  return ({ tag: "right", value: [LibLists.concat2(types)(LibLists.pure(t)), cxB] });
})();
})()));
})();
})())))))(({ tag: "right", value: [[], cx] }))(LibSets.toList(els));
  return LibEithers.bind(foldResult)(((foldR: readonly [ReadonlyArray<Core.Type>, Context.Context]) => (() => {
  const eltypes = LibPairs.first(foldR);
  return (() => {
  const cx2 = LibPairs.second(foldR);
  return LibEithers.bind(checkSameType(cx2)(tx)("set elements")(eltypes))(((unifiedType: Core.Type) => ({ tag: "right", value: [({ tag: "set", value: unifiedType }), cx2] })));
})();
})()));
})()))));
}

export function typeOfTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: Core.Term) => Errors.Error | Core.Type)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => LibEithers.map(LibPairs.first)(typeOf(cx)(g)([])(term))));
}

export function typeOfTypeApplication(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.TypeApplicationTerm) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((tyapp: Core.TypeApplicationTerm) => (() => {
  const body = ((_x) => _x.body)(tyapp);
  return (() => {
  const t = ((_x) => _x.type)(tyapp);
  return typeOf(cx)(tx)(LibLists.cons(t)(typeArgs))(body);
})();
})())));
}

export function typeOfTypeLambda(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.TypeLambda) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((tl: Core.TypeLambda) => (() => {
  const v = ((_x) => _x.parameter)(tl);
  return (() => {
  const body = ((_x) => _x.body)(tl);
  return (() => {
  const vars = ((_x) => _x.typeVariables)(tx);
  return (() => {
  const tx2 = ({
    boundTerms: ((_x) => _x.boundTerms)(tx),
    boundTypes: ((_x) => _x.boundTypes)(tx),
    classConstraints: ((_x) => _x.classConstraints)(tx),
    lambdaVariables: ((_x) => _x.lambdaVariables)(tx),
    metadata: ((_x) => _x.metadata)(tx),
    primitives: ((_x) => _x.primitives)(tx),
    schemaTypes: ((_x) => _x.schemaTypes)(tx),
    typeVariables: LibSets.insert(v)(vars)
  });
  return LibEithers.bind(typeOf(cx)(tx2)([])(body))(((result1: readonly [Core.Type, Context.Context]) => (() => {
  const t1 = LibPairs.first(result1);
  return (() => {
  const cx2 = LibPairs.second(result1);
  return LibEithers.bind(applyTypeArgumentsToType(cx2)(tx)(typeArgs)(({ tag: "forall", value: ({
    parameter: v,
    body: t1
  }) })))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx2] })));
})();
})()));
})();
})();
})();
})())));
}

export function typeOfUnit<t0>(cx: t0): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => Errors.Error | readonly [Core.Type, t0])) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => LibEithers.bind(applyTypeArgumentsToType(cx)(tx)(typeArgs)(({ tag: "unit" })))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx] })))));
}

export function typeOfUnwrap(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Name) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((tname: Core.Name) => LibEithers.bind(Resolution.requireSchemaType(cx)(((_x) => _x.schemaTypes)(tx))(tname))(((schemaResult: readonly [Core.TypeScheme, Context.Context]) => (() => {
  const schemaType = LibPairs.first(schemaResult);
  return (() => {
  const cx2 = LibPairs.second(schemaResult);
  return (() => {
  const svars = ((_x) => _x.variables)(schemaType);
  return (() => {
  const sbody = ((_x) => _x.type)(schemaType);
  return LibEithers.bind(ExtractCore.wrappedType(tname)(sbody))(((wrapped: Core.Type) => (() => {
  const subst = LibMaps.fromList(LibLists.zip(svars)(typeArgs));
  return (() => {
  const swrapped = Substitution.substInType(subst)(wrapped);
  return ({ tag: "right", value: [({ tag: "function", value: ({
    domain: Resolution.nominalApplication(tname)(typeArgs),
    codomain: swrapped
  }) }), cx2] });
})();
})()));
})();
})();
})();
})())))));
}

export function typeOfVariable(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Name) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((name: Core.Name) => (() => {
  const rawTypeScheme = LibMaps.lookup(name)(((_x) => _x.boundTypes)(tx));
  return (() => {
  const forScheme = ((ts: Core.TypeScheme) => (() => {
  const tResult = LibLogic.ifElse(LibLists.null_(typeArgs))(Resolution.instantiateType(cx)(Scoping.typeSchemeToFType(ts)))([Scoping.typeSchemeToFType(ts), cx]);
  return (() => {
  const t = LibPairs.first(tResult);
  return (() => {
  const cx2 = LibPairs.second(tResult);
  return LibEithers.bind(applyTypeArgumentsToType(cx2)(tx)(typeArgs)(t))(((applied: Core.Type) => ({ tag: "right", value: [applied, cx2] })));
})();
})();
})());
  return LibMaybes.maybe(LibMaybes.maybe(({ tag: "left", value: ({ tag: "untypedTermVariable", value: ({
    location: [],
    name: name
  }) }) }))(forScheme)(LibMaybes.map(((_p: Graph.Primitive) => ((_x) => _x.type)(_p)))(LibMaps.lookup(name)(((_x) => _x.primitives)(tx)))))(forScheme)(rawTypeScheme);
})();
})())));
}

export function typeOfWrappedTerm(cx: Context.Context): ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.WrappedTerm) => Errors.Error | readonly [Core.Type, Context.Context]))) {
  return ((tx: Graph.Graph) => ((typeArgs: ReadonlyArray<Core.Type>) => ((wt: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x.typeName)(wt);
  return (() => {
  const body = ((_x) => _x.body)(wt);
  return LibEithers.bind(typeOf(cx)(tx)([])(body))(((result: readonly [Core.Type, Context.Context]) => (() => {
  const cx2 = LibPairs.second(result);
  return ({ tag: "right", value: [Resolution.nominalApplication(tname)(typeArgs), cx2] });
})()));
})();
})())));
}

export function typesAllEffectivelyEqual(tx: Graph.Graph): ((x: ReadonlyArray<Core.Type>) => boolean) {
  return ((tlist: ReadonlyArray<Core.Type>) => (() => {
  const types = ((_x) => _x.schemaTypes)(tx);
  return (() => {
  const containsFreeVar = ((t: Core.Type) => (() => {
  const allVars = Variables.freeVariablesInTypeSimple(t);
  return (() => {
  const schemaNames = LibSets.fromList(LibMaps.keys(types));
  return LibLogic.not(LibSets.null_(LibSets.difference(allVars)(schemaNames)));
})();
})());
  return (() => {
  const anyContainsFreeVar = LibLists.foldl(((acc: boolean) => ((t: Core.Type) => LibLogic.or(acc)(containsFreeVar(t)))))(false)(tlist);
  return LibLogic.ifElse(anyContainsFreeVar)(true)(LibLogic.ifElse(allEqual(LibLists.map(((t: Core.Type) => normalizeTypeFreeVars(t)))(tlist)))(true)(allEqual(LibLists.map(((t: Core.Type) => normalizeTypeFreeVars(Strip.deannotateTypeRecursive(Dependencies.replaceTypedefs(types)(t)))))(tlist))));
})();
})();
})());
}

export function typesEffectivelyEqual(tx: Graph.Graph): ((x: Core.Type) => ((x: Core.Type) => boolean)) {
  return ((t1: Core.Type) => ((t2: Core.Type) => LibLogic.or(containsInScopeTypeVars(tx)(t1))(LibLogic.or(containsInScopeTypeVars(tx)(t2))(typesAllEffectivelyEqual(tx)([Resolution.fullyStripAndNormalizeType(t1), Resolution.fullyStripAndNormalizeType(t2)])))));
}
