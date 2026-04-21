// Note: this is an automatically generated file. Do not edit.

/**
 * Python code generator: converts Hydra modules to Python source code
 */



import * as Analysis from "../analysis.js";
import * as Annotations from "../annotations.js";
import * as Arity from "../arity.js";
import * as Ast from "../ast.js";
import * as Checking from "../checking.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as Dependencies from "../dependencies.js";
import * as Environment from "../environment.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as Inference from "../inference.js";
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
import * as PythonEnvironment from "./environment.js";
import * as PythonNames from "./names.js";
import * as PythonSerde from "./serde.js";
import * as PythonSyntax from "./syntax.js";
import * as PythonUtils from "./utils.js";
import * as Query from "../query.js";
import * as Reduction from "../reduction.js";
import * as Relational from "../relational.js";
import * as Resolution from "../resolution.js";
import * as Rewriting from "../rewriting.js";
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

export function analyzePythonFunction<t0>(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Term) => t0 | Typing.FunctionStructure<PythonEnvironment.PythonEnvironment>)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((term: Core.Term) => Analysis.analyzeFunctionTermWith(cx)(pythonBindingMetadata)(pythonEnvironmentGetGraph)(pythonEnvironmentSetGraph)(env)(term)));
}

export function classVariantPatternUnit(pyVariantName: PythonSyntax.Name): PythonSyntax.ClosedPattern {
  return ({ tag: "class", value: ({
    nameOrAttribute: [pyVariantName],
    positionalPatterns: null,
    keywordPatterns: null
  }) });
}

export function classVariantPatternWithCapture(env: PythonEnvironment.PythonEnvironment): ((x: PythonSyntax.Name) => ((x: Core.Name) => PythonSyntax.ClosedPattern)) {
  return ((pyVariantName: PythonSyntax.Name) => ((varName: Core.Name) => (() => {
  const pyVarNameAttr = [pyVariantName];
  return (() => {
  const capturePattern = ({ tag: "capture", value: PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(varName) });
  return (() => {
  const keywordPattern = ({
    name: "value",
    pattern: ({ tag: "or", value: [capturePattern] })
  });
  return ({ tag: "class", value: ({
    nameOrAttribute: pyVarNameAttr,
    positionalPatterns: null,
    keywordPatterns: [keywordPattern]
  }) });
})();
})();
})()));
}

export function collectTypeVariables(initial: ReadonlySet<Core.Name>): ((x: Core.Type) => ReadonlySet<Core.Name>) {
  return ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(ft);
  return (() => {
  const body = ((_x) => _x.body)(ft);
  return collectTypeVariables(LibSets.insert(v)(initial))(body);
})();
})())((_m as any).value);
    default: return (() => {
  const freeVars = Variables.freeVariablesInType(typ);
  return (() => {
  const isTypeVar = ((n: Core.Name) => isTypeVariableName(n));
  return (() => {
  const filteredList = LibLists.filter(isTypeVar)(LibSets.toList(freeVars));
  return LibSets.union(initial)(LibSets.fromList(filteredList));
})();
})();
})()(_m);
  }
})());
}

export function condImportSymbol<t0>(name: t0): ((x: boolean) => t0 | null) {
  return ((flag: boolean) => LibLogic.ifElse(flag)(name)(null));
}

export const dataclassDecorator: PythonSyntax.NamedExpression = ({ tag: "simple", value: PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithRhs(({ tag: "simple", value: ({ tag: "name", value: "dataclass" }) }))(({ tag: "call", value: ({
    positional: [],
    kwargOrStarred: [({ tag: "kwarg", value: ({
    name: "frozen",
    value: PythonUtils.pyAtomToPyExpression(({ tag: "true" }))
  }) })],
    kwargOrDoubleStarred: []
  }) }))) });

export function deconflictVariantName(isQualified: boolean): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.Name) => ((x: Graph.Graph) => PythonSyntax.Name)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((unionName: Core.Name) => ((fname: Core.Name) => ((g: Graph.Graph) => (() => {
  const candidateHydraName = LibStrings.cat2(((_x) => _x)(unionName))(Formatting.capitalize(((_x) => _x)(fname)));
  return (() => {
  const termCollision = LibMaps.member(candidateHydraName)(((_x) => _x.boundTerms)(g));
  return (() => {
  const typeCollision = LibMaps.member(candidateHydraName)(((_x) => _x.schemaTypes)(g));
  return (() => {
  const collision = LibLogic.or(termCollision)(typeCollision);
  return LibLogic.ifElse(collision)(LibStrings.cat2(((_x) => _x)(PythonNames.variantName(isQualified)(env)(unionName)(fname)))("_"))(PythonNames.variantName(isQualified)(env)(unionName)(fname));
})();
})();
})();
})()))));
}

export function deduplicateCaseVariables(cases_: ReadonlyArray<Core.Field>): ReadonlyArray<Core.Field> {
  return (() => {
  const rewriteCase = ((state: readonly [ReadonlyMap<Core.Name, number>, ReadonlyArray<Core.Field>]) => ((field: Core.Field) => (() => {
  const countByName = LibPairs.first(state);
  return (() => {
  const done = LibPairs.second(state);
  return (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(fterm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const v = ((_x) => _x.parameter)(lam);
  return (() => {
  const mdom = ((_x) => _x.domain)(lam);
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return LibMaybes.maybe([LibMaps.insert(v)(1)(countByName), LibLists.cons(field)(done)])(((count: number) => (() => {
  const count2 = LibMath.add(count)(1);
  return (() => {
  const v2 = LibStrings.cat2(((_x) => _x)(v))(LibLiterals.showInt32(count2));
  return (() => {
  const newBody = Reduction.alphaConvert(v)(v2)(body);
  return (() => {
  const newLam = ({
    parameter: v2,
    domain: mdom,
    body: newBody
  });
  return (() => {
  const newTerm = ({ tag: "lambda", value: newLam });
  return (() => {
  const newField = ({
    name: fname,
    term: newTerm
  });
  return [LibMaps.insert(v)(count2)(countByName), LibLists.cons(newField)(done)];
})();
})();
})();
})();
})();
})()))(LibMaps.lookup(v)(countByName));
})();
})();
})())((_m as any).value);
    default: return [countByName, LibLists.cons(field)(done)](_m);
  }
})();
})();
})();
})();
})()));
  return (() => {
  const result = LibLists.foldl(rewriteCase)([LibMaps.empty, []])(cases_);
  return LibLists.reverse(LibPairs.second(result));
})();
})();
}

export function digForWrap(isTermAnnot: boolean): ((x: PythonEnvironment.PythonModuleMetadata) => ((x: Core.Type) => PythonEnvironment.PythonModuleMetadata)) {
  return ((meta: PythonEnvironment.PythonModuleMetadata) => ((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => digForWrap(isTermAnnot)(meta)(((_x) => _x.body)(ft)))((_m as any).value);
    case "wrap": return ((_: Core.Type) => LibLogic.ifElse(isTermAnnot)(meta)(setMetaUsesNode(meta)(true)))((_m as any).value);
    default: return meta(_m);
  }
})()));
}

export function eliminateUnitVar(v: Core.Name): ((x: Core.Term) => Core.Term) {
  return ((term0: Core.Term) => (() => {
  const rewriteField = ((rewrite: ((x: Core.Term) => Core.Term)) => ((fld: Core.Field) => ({
    name: ((_x) => _x.name)(fld),
    term: rewrite(((_x) => _x.term)(fld))
  })));
  return (() => {
  const rewriteBinding = ((rewrite: ((x: Core.Term) => Core.Term)) => ((bnd: Core.Binding) => ({
    name: ((_x) => _x.name)(bnd),
    term: rewrite(((_x) => _x.term)(bnd)),
    type: ((_x) => _x.type)(bnd)
  })));
  return (() => {
  const rewrite = ((recurse: ((x: Core.Term) => Core.Term)) => ((term: Core.Term) => (() => {
  const _m = Strip.deannotateAndDetypeTerm(term);
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => LibLogic.ifElse(LibEquality.equal(n)(v))(({ tag: "unit" }))(term))((_m as any).value);
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: recurse(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "application": return ((app: Core.Application) => ({ tag: "application", value: ({
    function: recurse(((_x) => _x.function)(app)),
    argument: recurse(((_x) => _x.argument)(app))
  }) }))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.parameter)(lam))(v))(term)(({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(lam),
    domain: ((_x) => _x.domain)(lam),
    body: recurse(((_x) => _x.body)(lam))
  }) })))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(recurse)(((_x) => _x.default)(cs)),
    cases: LibLists.map(((v1: Core.Field) => rewriteField(recurse)(v1)))(((_x) => _x.cases)(cs))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: ({
    bindings: LibLists.map(((v1: Core.Binding) => rewriteBinding(recurse)(v1)))(((_x) => _x.bindings)(lt)),
    body: recurse(((_x) => _x.body)(lt))
  }) }))((_m as any).value);
    case "list": return ((ts: ReadonlyArray<Core.Term>) => ({ tag: "list", value: LibLists.map(recurse)(ts) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => ({ tag: "map", value: LibMaps.fromList(LibLists.map(((kv: readonly [Core.Term, Core.Term]) => [recurse(LibPairs.first(kv)), recurse(LibPairs.second(kv))]))(LibMaps.toList(m))) }))((_m as any).value);
    case "record": return ((rec: Core.Record) => ({ tag: "record", value: ({
    typeName: ((_x) => _x.typeName)(rec),
    fields: LibLists.map(((v1: Core.Field) => rewriteField(recurse)(v1)))(((_x) => _x.fields)(rec))
  }) }))((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => ({ tag: "set", value: LibSets.map(recurse)(s) }))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => ({ tag: "inject", value: ({
    typeName: ((_x) => _x.typeName)(inj),
    field: rewriteField(recurse)(((_x) => _x.field)(inj))
  }) }))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => ({ tag: "maybe", value: LibMaybes.map(recurse)(mt) }))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => ({ tag: "pair", value: [recurse(LibPairs.first(p)), recurse(LibPairs.second(p))] }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => ({ tag: "wrap", value: ({
    typeName: ((_x) => _x.typeName)(wt),
    body: recurse(((_x) => _x.body)(wt))
  }) }))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => ({ tag: "either", value: LibEithers.bimap(recurse)(recurse)(e) }))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: recurse(((_x) => _x.body)(ta)),
    type: ((_x) => _x.type)(ta)
  }) }))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: recurse(((_x) => _x.body)(tl))
  }) }))((_m as any).value);
    default: return term(_m);
  }
})()));
  return (() => {
  const go = ((term: Core.Term) => rewrite(go)(term));
  return go(term0);
})();
})();
})();
})());
}

export function emptyMetadata(ns: Packaging.Namespaces<PythonSyntax.DottedName>): PythonEnvironment.PythonModuleMetadata {
  return ({
    namespaces: ns,
    typeVariables: LibSets.empty,
    usesAnnotated: false,
    usesCallable: false,
    usesCast: false,
    usesLruCache: false,
    usesTypeAlias: false,
    usesDataclass: false,
    usesDecimal: false,
    usesEither: false,
    usesEnum: false,
    usesFrozenDict: false,
    usesFrozenList: false,
    usesGeneric: false,
    usesJust: false,
    usesLeft: false,
    usesMaybe: false,
    usesName: false,
    usesNode: false,
    usesNothing: false,
    usesRight: false,
    usesTypeVar: false
  });
}

export function encodeApplication(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Application) => Errors.Error | PythonSyntax.Expression)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((app: Core.Application) => (() => {
  const g = pythonEnvironmentGetGraph(env);
  return (() => {
  const term = ({ tag: "application", value: app });
  return (() => {
  const gathered = Analysis.gatherArgs(term)([]);
  return (() => {
  const fun = LibPairs.first(gathered);
  return (() => {
  const args = LibPairs.second(gathered);
  return (() => {
  const knownArity = termArityWithPrimitives(g)(fun);
  return (() => {
  const arity = LibMath.max(knownArity)(LibLists.length(args));
  return LibEithers.bind(LibEithers.mapList(((t: Core.Term) => encodeTermInline(cx)(env)(false)(t)))(args))(((pargs: ReadonlyArray<PythonSyntax.Expression>) => (() => {
  const hargs = LibLists.take(arity)(pargs);
  return (() => {
  const rargs = LibLists.drop(arity)(pargs);
  return LibEithers.bind(encodeApplicationInner(cx)(env)(fun)(hargs)(rargs))(((result: readonly [PythonSyntax.Expression, ReadonlyArray<PythonSyntax.Expression>]) => (() => {
  const lhs = LibPairs.first(result);
  return (() => {
  const remainingRargs = LibPairs.second(result);
  return (() => {
  const pyapp = LibLists.foldl(((t: PythonSyntax.Expression) => ((a: PythonSyntax.Expression) => PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(t))([a]))))(lhs)(remainingRargs);
  return ({ tag: "right", value: pyapp });
})();
})();
})()));
})();
})()));
})();
})();
})();
})();
})();
})();
})()));
}

export function encodeApplicationInner(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Term) => ((x: ReadonlyArray<PythonSyntax.Expression>) => ((x: ReadonlyArray<PythonSyntax.Expression>) => Errors.Error | readonly [PythonSyntax.Expression, ReadonlyArray<PythonSyntax.Expression>])))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((fun: Core.Term) => ((hargs: ReadonlyArray<PythonSyntax.Expression>) => ((rargs: ReadonlyArray<PythonSyntax.Expression>) => (() => {
  const firstArg = LibLists.head(hargs);
  return (() => {
  const restArgs = LibLists.tail(hargs);
  return (() => {
  const withRest = ((e: PythonSyntax.Expression) => LibLogic.ifElse(LibLists.null_(restArgs))(e)(PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(e))(restArgs)));
  return (() => {
  const defaultCase = LibEithers.bind(encodeTermInline(cx)(env)(false)(fun))(((pfun: PythonSyntax.Expression) => ({ tag: "right", value: [PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(pfun))(hargs), rargs] })));
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(fun);
  switch (_m.tag) {
    case "project": return ((proj: Core.Projection) => (() => {
  const fname = ((_x) => _x.field)(proj);
  return (() => {
  const fieldExpr = PythonUtils.projectFromExpression(firstArg)(PythonNames.encodeFieldName(env)(fname));
  return ({ tag: "right", value: [withRest(fieldExpr), rargs] });
})();
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => LibEithers.bind(encodeUnionEliminationInline(cx)(env)(cs)(firstArg))(((inlineExpr: PythonSyntax.Expression) => ({ tag: "right", value: [withRest(inlineExpr), rargs] }))))((_m as any).value);
    case "unwrap": return ((_: Core.Name) => (() => {
  const valueExpr = PythonUtils.projectFromExpression(firstArg)("value");
  return (() => {
  const allArgs = LibLists.concat2(restArgs)(rargs);
  return LibLogic.ifElse(LibLists.null_(allArgs))(({ tag: "right", value: [valueExpr, []] }))(({ tag: "right", value: [PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(valueExpr))(allArgs), []] }));
})();
})())((_m as any).value);
    case "lambda": return ((_: Core.Lambda) => LibEithers.bind(encodeTermInline(cx)(env)(false)(fun))(((pfun: PythonSyntax.Expression) => ({ tag: "right", value: [PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(pfun))(hargs), rargs] }))))((_m as any).value);
    case "variable": return ((name: Core.Name) => (() => {
  const g = pythonEnvironmentGetGraph(env);
  return (() => {
  const allArgs = LibLists.concat2(hargs)(rargs);
  return LibMaybes.cases(LibMaps.lookup(name)(((_x) => _x.primitives)(g)))(LibMaybes.maybe(LibEithers.bind(encodeVariable(cx)(env)(name)(hargs))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [expr, rargs] }))))(((el: Core.Binding) => LibMaybes.maybe(LibEithers.bind(encodeVariable(cx)(env)(name)(hargs))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [expr, rargs] }))))(((ts: Core.TypeScheme) => (() => {
  const elArity = Arity.typeSchemeArity(ts);
  return (() => {
  const consumeCount = LibMath.min(elArity)(LibLists.length(allArgs));
  return (() => {
  const consumedArgs = LibLists.take(consumeCount)(allArgs);
  return (() => {
  const remainingArgs = LibLists.drop(consumeCount)(allArgs);
  return LibLogic.ifElse(LibLists.null_(consumedArgs))(LibEithers.bind(encodeVariable(cx)(env)(name)([]))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [expr, rargs] }))))(({ tag: "right", value: [PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "lowerSnake" }))(env)(name)))(consumedArgs), remainingArgs] }));
})();
})();
})();
})()))(((_x) => _x.type)(el))))(Lexical.lookupBinding(g)(name)))(((_prim: Graph.Primitive) => (() => {
  const wrappedArgs = wrapLazyArguments(name)(hargs);
  return LibEithers.bind(encodeVariable(cx)(env)(name)(wrappedArgs))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [expr, rargs] })));
})()));
})();
})())((_m as any).value);
    default: return defaultCase(_m);
  }
})();
})();
})();
})();
})()))));
}

export function encodeApplicationType<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.ApplicationType) => t0 | PythonSyntax.Expression) {
  return ((at: Core.ApplicationType) => (() => {
  const gatherParams = ((t: Core.Type) => ((ps: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "application": return ((appT: Core.ApplicationType) => gatherParams(((_x) => _x.function)(appT))(LibLists.cons(((_x) => _x.argument)(appT))(ps)))((_m as any).value);
    case "annotated": return ((_: Core.AnnotatedType) => [t, ps])((_m as any).value);
    case "function": return ((_: Core.FunctionType) => [t, ps])((_m as any).value);
    case "forall": return ((_: Core.ForallType) => [t, ps])((_m as any).value);
    case "list": return ((_: Core.Type) => [t, ps])((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => [t, ps])((_m as any).value);
    case "map": return ((_: Core.MapType) => [t, ps])((_m as any).value);
    case "maybe": return ((_: Core.Type) => [t, ps])((_m as any).value);
    case "either": return ((_: Core.EitherType) => [t, ps])((_m as any).value);
    case "pair": return ((_: Core.PairType) => [t, ps])((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => [t, ps])((_m as any).value);
    case "set": return ((_: Core.Type) => [t, ps])((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => [t, ps])((_m as any).value);
    case "unit": return ((_: void) => [t, ps])((_m as any).value);
    case "variable": return ((_: Core.Name) => [t, ps])((_m as any).value);
    case "void": return ((_: void) => [t, ps])((_m as any).value);
    case "wrap": return ((_: Core.Type) => [t, ps])((_m as any).value);
  }
})()));
  return (() => {
  const bodyAndArgs = gatherParams(({ tag: "application", value: at }))([]);
  return (() => {
  const body = LibPairs.first(bodyAndArgs);
  return (() => {
  const args = LibPairs.second(bodyAndArgs);
  return LibEithers.bind(encodeType(env)(body))(((pyBody: PythonSyntax.Expression) => LibEithers.bind(LibEithers.mapList(((v1: Core.Type) => encodeType(env)(v1)))(args))(((pyArgs: ReadonlyArray<PythonSyntax.Expression>) => ({ tag: "right", value: PythonUtils.primaryAndParams(PythonUtils.pyExpressionToPyPrimary(pyBody))(pyArgs) })))));
})();
})();
})();
})());
}

export function encodeBindingAs(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Binding) => Errors.Error | PythonSyntax.Statement)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((binding: Core.Binding) => (() => {
  const name1 = ((_x) => _x.name)(binding);
  return (() => {
  const term1 = ((_x) => _x.term)(binding);
  return (() => {
  const mts = ((_x) => _x.type)(binding);
  return (() => {
  const fname = PythonNames.encodeName(true)(({ tag: "lowerSnake" }))(env)(name1);
  return LibMaybes.maybe((() => {
  const gathered = gatherLambdas(term1);
  return (() => {
  const lambdaParams = LibPairs.first(gathered);
  return (() => {
  const innerBody = LibPairs.second(gathered);
  return (() => {
  const mcsa = isCaseStatementApplication(innerBody);
  return LibMaybes.maybe((() => {
  const mcs = extractCaseElimination(term1);
  return LibMaybes.maybe(LibEithers.map(((stmts: ReadonlyArray<PythonSyntax.Statement>) => LibLists.head(stmts)))(encodeTermMultiline(cx)(env)(term1)))(((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const isFull = isCasesFull(rt)(cases_);
  return (() => {
  const innerParam = ({
    name: "x",
    annotation: null
  });
  return (() => {
  const param = ({
    param: innerParam,
    typeComment: null
  });
  return (() => {
  const params = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: [param],
    paramWithDefault: [],
    starEtc: null
  }) });
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)(((e: PythonEnvironment.PythonEnvironment) => ((t: Core.Term) => encodeTermMultiline(cx)(e)(t))))(v1)))(cases_))(((pyCases: ReadonlyArray<PythonSyntax.CaseBlock>) => LibEithers.bind(encodeDefaultCaseBlock(((t: Core.Term) => encodeTermInline(cx)(env)(false)(t)))(isFull)(dflt)(tname))(((pyDflt: ReadonlyArray<PythonSyntax.CaseBlock>) => (() => {
  const subj = ({ tag: "simple", value: ({ tag: "simple", value: PythonUtils.pyNameToPyExpression("x") }) });
  return (() => {
  const allCases = LibLists.concat2(pyCases)(pyDflt);
  return (() => {
  const matchStmt = ({ tag: "compound", value: ({ tag: "match", value: ({
    subject: subj,
    cases: allCases
  }) }) });
  return (() => {
  const body = PythonUtils.indentedBlock(null)([[matchStmt]]);
  return (() => {
  const funcDefRaw = ({
    async: false,
    name: fname,
    typeParams: [],
    params: params,
    returnType: null,
    funcTypeComment: null,
    block: body
  });
  return ({ tag: "right", value: ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: funcDefRaw
  }) }) }) });
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})()));
})();
})();
})()))(mcs);
})())(((csa: readonly [Core.Name, readonly [Core.Term | null, readonly [ReadonlyArray<Core.Field>, Core.Term]]]) => LibLogic.ifElse(LibLists.null_(lambdaParams))((() => {
  const mcs = extractCaseElimination(term1);
  return LibMaybes.maybe(LibEithers.map(((stmts: ReadonlyArray<PythonSyntax.Statement>) => LibLists.head(stmts)))(encodeTermMultiline(cx)(env)(term1)))(((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const isFull = isCasesFull(rt)(cases_);
  return (() => {
  const innerParam = ({
    name: "x",
    annotation: null
  });
  return (() => {
  const param = ({
    param: innerParam,
    typeComment: null
  });
  return (() => {
  const params = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: [param],
    paramWithDefault: [],
    starEtc: null
  }) });
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)(((e: PythonEnvironment.PythonEnvironment) => ((t: Core.Term) => encodeTermMultiline(cx)(e)(t))))(v1)))(cases_))(((pyCases: ReadonlyArray<PythonSyntax.CaseBlock>) => LibEithers.bind(encodeDefaultCaseBlock(((t: Core.Term) => encodeTermInline(cx)(env)(false)(t)))(isFull)(dflt)(tname))(((pyDflt: ReadonlyArray<PythonSyntax.CaseBlock>) => (() => {
  const subj = ({ tag: "simple", value: ({ tag: "simple", value: PythonUtils.pyNameToPyExpression("x") }) });
  return (() => {
  const allCases = LibLists.concat2(pyCases)(pyDflt);
  return (() => {
  const matchStmt = ({ tag: "compound", value: ({ tag: "match", value: ({
    subject: subj,
    cases: allCases
  }) }) });
  return (() => {
  const body = PythonUtils.indentedBlock(null)([[matchStmt]]);
  return (() => {
  const funcDefRaw = ({
    async: false,
    name: fname,
    typeParams: [],
    params: params,
    returnType: null,
    funcTypeComment: null,
    block: body
  });
  return ({ tag: "right", value: ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: funcDefRaw
  }) }) }) });
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})()));
})();
})();
})()))(mcs);
})())((() => {
  const tname = LibPairs.first(csa);
  return (() => {
  const rest1 = LibPairs.second(csa);
  return (() => {
  const dflt = LibPairs.first(rest1);
  return (() => {
  const rest2 = LibPairs.second(rest1);
  return (() => {
  const cases_ = LibPairs.first(rest2);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const isFull = isCasesFull(rt)(cases_);
  return (() => {
  const capturedVarNames = LibLists.init(lambdaParams);
  return (() => {
  const matchLambdaParam = LibLists.last(lambdaParams);
  return (() => {
  const capturedParams = LibLists.map(((n: Core.Name) => ({
    param: ({
    name: PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(n),
    annotation: null
  }),
    typeComment: null
  })))(capturedVarNames);
  return (() => {
  const matchArgName = PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(matchLambdaParam);
  return (() => {
  const matchParam = ({
    param: ({
    name: matchArgName,
    annotation: null
  }),
    typeComment: null
  });
  return (() => {
  const allParams = LibLists.concat2(capturedParams)([matchParam]);
  return (() => {
  const params = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: allParams,
    paramWithDefault: [],
    starEtc: null
  }) });
  return (() => {
  const envWithParams = extendEnvWithLambdaParams(env)(term1);
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => encodeCaseBlock(cx)(envWithParams)(tname)(rt)(isEnum)(((e: PythonEnvironment.PythonEnvironment) => ((t: Core.Term) => encodeTermMultiline(cx)(e)(t))))(v1)))(cases_))(((pyCases: ReadonlyArray<PythonSyntax.CaseBlock>) => LibEithers.bind(encodeDefaultCaseBlock(((t: Core.Term) => encodeTermInline(cx)(envWithParams)(false)(t)))(isFull)(dflt)(tname))(((pyDflt: ReadonlyArray<PythonSyntax.CaseBlock>) => (() => {
  const subj = ({ tag: "simple", value: ({ tag: "simple", value: PythonUtils.pyNameToPyExpression(matchArgName) }) });
  return (() => {
  const allCases = LibLists.concat2(pyCases)(pyDflt);
  return (() => {
  const matchStmt = ({ tag: "compound", value: ({ tag: "match", value: ({
    subject: subj,
    cases: allCases
  }) }) });
  return (() => {
  const body = PythonUtils.indentedBlock(null)([[matchStmt]]);
  return (() => {
  const funcDefRaw = ({
    async: false,
    name: fname,
    typeParams: [],
    params: params,
    returnType: null,
    funcTypeComment: null,
    block: body
  });
  return ({ tag: "right", value: ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: null,
    raw: funcDefRaw
  }) }) }) });
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})();
})())))(mcsa);
})();
})();
})();
})())(((ts: Core.TypeScheme) => LibEithers.bind(Annotations.getTermDescription(cx)(pythonEnvironmentGetGraph(env))(term1))(((comment: string | null) => (() => {
  const normComment = LibMaybes.map(Formatting.normalizeComment)(comment);
  return encodeTermAssignment(cx)(env)(name1)(term1)(ts)(normComment);
})()))))(mts);
})();
})();
})();
})()));
}

export function encodeBindingAsAssignment(cx: Context.Context): ((x: boolean) => ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Binding) => Errors.Error | PythonSyntax.NamedExpression))) {
  return ((allowThunking: boolean) => ((env: PythonEnvironment.PythonEnvironment) => ((binding: Core.Binding) => (() => {
  const name = ((_x) => _x.name)(binding);
  return (() => {
  const term = ((_x) => _x.term)(binding);
  return (() => {
  const mts = ((_x) => _x.type)(binding);
  return (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(name);
  return LibEithers.bind(encodeTermInline(cx)(env)(false)(term))(((pbody: PythonSyntax.Expression) => (() => {
  const tc = ((_x) => _x.graph)(env);
  return (() => {
  const isComplexVar = Predicates.isComplexVariable(tc)(name);
  return (() => {
  const termIsComplex = Predicates.isComplexTerm(tc)(term);
  return (() => {
  const isTrivial = Predicates.isTrivialTerm(term);
  return (() => {
  const needsThunk = LibLogic.ifElse(isTrivial)(false)(LibMaybes.maybe(LibLogic.and(allowThunking)(LibLogic.or(isComplexVar)(termIsComplex)))(((ts: Core.TypeScheme) => LibLogic.and(allowThunking)(LibLogic.and(LibEquality.equal(Arity.typeSchemeArity(ts))(0))(LibLogic.or(isComplexVar)(termIsComplex)))))(mts));
  return (() => {
  const pterm = LibLogic.ifElse(needsThunk)(makeThunk(pbody))(pbody);
  return ({ tag: "right", value: ({ tag: "assignment", value: ({
    name: pyName,
    expression: pterm
  }) }) });
})();
})();
})();
})();
})();
})()));
})();
})();
})();
})())));
}

export function encodeBindingsAsDefs<t0, t1, t2, t3>(env: t0): ((x: ((x: t0) => ((x: t1) => t2 | t3))) => ((x: ReadonlyArray<t1>) => t2 | ReadonlyArray<t3>)) {
  return ((encodeBinding: ((x: t0) => ((x: t1) => t2 | t3))) => ((bindings: ReadonlyArray<t1>) => LibEithers.mapList(((v1: t1) => encodeBinding(env)(v1)))(bindings)));
}

export function encodeCaseBlock<t0, t1>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: boolean) => ((x: ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Term) => t1 | ReadonlyArray<PythonSyntax.Statement>))) => ((x: Core.Field) => t1 | PythonSyntax.CaseBlock)))))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((tname: Core.Name) => ((rowType: ReadonlyArray<Core.FieldType>) => ((isEnum: boolean) => ((encodeBody: ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Term) => t1 | ReadonlyArray<PythonSyntax.Statement>))) => ((field: Core.Field) => (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const stripped = Strip.deannotateAndDetypeTerm(fterm);
  return (() => {
  const effectiveLambda = (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => lam)((_m as any).value);
    default: return (() => {
  const syntheticVar = "_matchValue";
  return ({
    parameter: syntheticVar,
    domain: null,
    body: ({ tag: "application", value: ({
    function: stripped,
    argument: ({ tag: "variable", value: syntheticVar })
  }) })
  });
})()(_m);
  }
})();
  return (() => {
  const v = ((_x) => _x.parameter)(effectiveLambda);
  return (() => {
  const rawBody = ((_x) => _x.body)(effectiveLambda);
  return (() => {
  const isUnitVariant = isVariantUnitType(rowType)(fname);
  return (() => {
  const effectiveBody = LibLogic.ifElse(isUnitVariant)(eliminateUnitVar(v)(rawBody))(rawBody);
  return (() => {
  const shouldCapture = LibLogic.not(LibLogic.or(isUnitVariant)(LibLogic.or(Variables.isFreeVariableInTerm(v)(rawBody))(Predicates.isUnitTerm(rawBody))));
  return (() => {
  const env2 = pythonEnvironmentSetGraph(Scoping.extendGraphForLambda(pythonEnvironmentGetGraph(env))(effectiveLambda))(env);
  return (() => {
  const pyVariantName = deconflictVariantName(true)(env2)(tname)(fname)(((_x) => _x.graph)(env2));
  return (() => {
  const pattern = variantClosedPattern(env2)(tname)(fname)(pyVariantName)(rowType)(isEnum)(v)(shouldCapture);
  return LibEithers.bind(encodeBody(env2)(effectiveBody))(((stmts: ReadonlyArray<PythonSyntax.Statement>) => (() => {
  const pyBody = PythonUtils.indentedBlock(null)([stmts]);
  return ({ tag: "right", value: ({
    patterns: PythonUtils.pyClosedPatternToPyPatterns(pattern),
    guard: null,
    body: pyBody
  }) });
})()));
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
})()))))));
}

export function encodeDefaultCaseBlock<t0, t1>(encodeTerm: ((x: t0) => t1 | PythonSyntax.Expression)): ((x: boolean) => ((x: t0 | null) => ((x: Core.Name) => t1 | ReadonlyArray<PythonSyntax.CaseBlock>))) {
  return ((isFull: boolean) => ((mdflt: t0 | null) => ((tname: Core.Name) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: LibLogic.ifElse(isFull)(PythonUtils.raiseAssertionError("Unreachable: all variants handled"))(PythonUtils.raiseTypeError(LibStrings.cat2("Unsupported ")(Names.localNameOf(tname)))) }))(((d: t0) => LibEithers.bind(encodeTerm(d))(((pyexpr: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.returnSingle(pyexpr) })))))(mdflt))(((stmt: PythonSyntax.Statement) => (() => {
  const patterns = PythonUtils.pyClosedPatternToPyPatterns(({ tag: "wildcard" }));
  return (() => {
  const body = PythonUtils.indentedBlock(null)([[stmt]]);
  return ({ tag: "right", value: [({
    patterns: patterns,
    guard: null,
    body: body
  })] });
})();
})())))));
}

export function encodeDefinition(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Packaging.Definition) => Errors.Error | ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((def_: Packaging.Definition) => (() => {
  const _m = def_;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => (() => {
  const name = ((_x) => _x.name)(td);
  return (() => {
  const term = ((_x) => _x.term)(td);
  return (() => {
  const typ = LibMaybes.maybe(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Unit" }),
    constraints: null
  }))(((x: Core.TypeScheme) => x))(((_x) => _x.type)(td));
  return LibEithers.bind(Annotations.getTermDescription(cx)(pythonEnvironmentGetGraph(env))(term))(((comment: string | null) => (() => {
  const normComment = LibMaybes.map(Formatting.normalizeComment)(comment);
  return LibEithers.bind(encodeTermAssignment(cx)(env)(name)(term)(typ)(normComment))(((stmt: PythonSyntax.Statement) => ({ tag: "right", value: [[stmt]] })));
})()));
})();
})();
})())((_m as any).value);
    case "type": return ((td: Packaging.TypeDefinition) => (() => {
  const name = ((_x) => _x.name)(td);
  return (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(td));
  return LibEithers.bind(Annotations.getTypeDescription(cx)(pythonEnvironmentGetGraph(env))(typ))(((comment: string | null) => (() => {
  const normComment = LibMaybes.map(Formatting.normalizeComment)(comment);
  return encodeTypeAssignment(cx)(env)(name)(typ)(normComment);
})()));
})();
})())((_m as any).value);
  }
})()));
}

export function encodeEnumValueAssignment<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.FieldType) => Errors.Error | ReadonlyArray<PythonSyntax.Statement>)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  return (() => {
  const ftype = ((_x) => _x.type)(fieldType);
  return LibEithers.bind(Annotations.getTypeDescription(cx)(pythonEnvironmentGetGraph(env))(ftype))(((mcomment: string | null) => (() => {
  const pyName = PythonNames.encodeEnumValue(env)(fname);
  return (() => {
  const fnameStr = ((_x) => _x)(fname);
  return (() => {
  const pyValue = PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "pascal" }))(env)("hydra.core.Name")))([PythonUtils.doubleQuotedString(fnameStr)]);
  return (() => {
  const assignStmt = PythonUtils.assignmentStatement(pyName)(pyValue);
  return ({ tag: "right", value: LibMaybes.maybe([assignStmt])(((c: string) => [assignStmt, PythonUtils.pyExpressionToPyStatement(PythonUtils.tripleQuotedString(c))]))(mcomment) });
})();
})();
})();
})()));
})();
})()));
}

export function encodeField<t0, t1, t2>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Field) => ((x: ((x: Core.Term) => t1 | t2)) => t1 | readonly [PythonSyntax.Name, t2]))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((field: Core.Field) => ((encodeTerm: ((x: Core.Term) => t1 | t2)) => (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return LibEithers.bind(encodeTerm(fterm))(((pterm: t2) => ({ tag: "right", value: [PythonNames.encodeFieldName(env)(fname), pterm] })));
})();
})())));
}

export function encodeFieldType<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.FieldType) => Errors.Error | PythonSyntax.Statement)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  return (() => {
  const ftype = ((_x) => _x.type)(fieldType);
  return LibEithers.bind(Annotations.getTypeDescription(cx)(pythonEnvironmentGetGraph(env))(ftype))(((comment: string | null) => (() => {
  const pyName = ({ tag: "name", value: PythonNames.encodeFieldName(env)(fname) });
  return LibEithers.bind(encodeType(env)(ftype))(((pyType: PythonSyntax.Expression) => (() => {
  const annotatedPyType = PythonUtils.annotatedExpression(comment)(pyType);
  return ({ tag: "right", value: PythonUtils.pyAssignmentToPyStatement(({ tag: "typed", value: ({
    lhs: pyName,
    type: annotatedPyType,
    rhs: null
  }) })) });
})()));
})()));
})();
})()));
}

export function encodeFloatValue<t0>(fv: Core.FloatValue): t0 | PythonSyntax.Expression {
  return (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((f: number) => ({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("Decimal"))([PythonUtils.singleQuotedString(LibLiterals.showBigfloat(f))]) }))((_m as any).value);
    case "float32": return ((f: number) => encodeFloatValue_encodeFloat32(f))((_m as any).value);
    case "float64": return ((f: number) => encodeFloatValue_encodeFloat64(f))((_m as any).value);
  }
})();
}

export function encodeFloatValue_encodeFloat32<t0>(v: number): t0 | PythonSyntax.Expression {
  return (() => {
  const s = LibLiterals.showFloat32(v);
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("nan") }))(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("inf") }))(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("-inf") }))(({ tag: "right", value: PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "float", value: LibLiterals.float32ToBigfloat(v) }) })) }))));
})();
}

export function encodeFloatValue_encodeFloat64<t0>(v: number): t0 | PythonSyntax.Expression {
  return (() => {
  const s = LibLiterals.showFloat64(v);
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("nan") }))(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("inf") }))(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("-inf") }))(LibLogic.ifElse(LibEquality.equal(s)("-0.0"))(({ tag: "right", value: encodeFloatValue_pySpecialFloat("-0.0") }))(({ tag: "right", value: PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "float", value: LibLiterals.float64ToBigfloat(v) }) })) })))));
})();
}

export function encodeFloatValue_pySpecialFloat(value: string): PythonSyntax.Expression {
  return PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("float"))([PythonUtils.singleQuotedString(value)]);
}

export function encodeForallType<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.ForallType) => t0 | PythonSyntax.Expression) {
  return ((lt: Core.ForallType) => (() => {
  const gatherParams = ((t: Core.Type) => ((ps: ReadonlyArray<Core.Name>) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((forallT: Core.ForallType) => gatherParams(((_x) => _x.body)(forallT))(LibLists.cons(((_x) => _x.parameter)(forallT))(ps)))((_m as any).value);
    case "annotated": return ((_: Core.AnnotatedType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "application": return ((_: Core.ApplicationType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "function": return ((_: Core.FunctionType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "list": return ((_: Core.Type) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "map": return ((_: Core.MapType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "maybe": return ((_: Core.Type) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "either": return ((_: Core.EitherType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "pair": return ((_: Core.PairType) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "set": return ((_: Core.Type) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "unit": return ((_: void) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "variable": return ((_: Core.Name) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "void": return ((_: void) => [t, LibLists.reverse(ps)])((_m as any).value);
    case "wrap": return ((_: Core.Type) => [t, LibLists.reverse(ps)])((_m as any).value);
  }
})()));
  return (() => {
  const bodyAndParams = gatherParams(({ tag: "forall", value: lt }))([]);
  return (() => {
  const body = LibPairs.first(bodyAndParams);
  return (() => {
  const params = LibPairs.second(bodyAndParams);
  return LibEithers.bind(encodeType(env)(body))(((pyBody: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.primaryAndParams(PythonUtils.pyExpressionToPyPrimary(pyBody))(LibLists.map(((n: Core.Name) => ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: ((_x) => _x)(n) }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })))(params)) })));
})();
})();
})();
})());
}

export function encodeFunctionDefinition(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyArray<Core.Name>) => ((x: Core.Term) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Type | null) => ((x: string | null) => ((x: ReadonlyArray<PythonSyntax.Statement>) => Errors.Error | PythonSyntax.Statement))))))))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((tparams: ReadonlyArray<Core.Name>) => ((args: ReadonlyArray<Core.Name>) => ((body: Core.Term) => ((doms: ReadonlyArray<Core.Type>) => ((mcod: Core.Type | null) => ((comment: string | null) => ((prefixes: ReadonlyArray<PythonSyntax.Statement>) => LibEithers.bind(LibEithers.mapList(((pair: readonly [Core.Name, Core.Type]) => (() => {
  const argName = LibPairs.first(pair);
  return (() => {
  const typ = LibPairs.second(pair);
  return LibEithers.bind(encodeType(env)(typ))(((pyTyp: PythonSyntax.Expression) => ({ tag: "right", value: ({
    param: ({
    name: PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(argName),
    annotation: pyTyp
  }),
    typeComment: null
  }) })));
})();
})()))(LibLists.zip(args)(doms)))(((pyArgs: ReadonlyArray<PythonSyntax.ParamNoDefault>) => (() => {
  const pyParams = ({ tag: "paramNoDefault", value: ({
    paramNoDefault: pyArgs,
    paramWithDefault: [],
    starEtc: null
  }) });
  return (() => {
  const isTCO = LibLogic.and(LibLogic.not(LibLists.null_(args)))(Analysis.isSelfTailRecursive(name)(body));
  return LibEithers.bind(LibLogic.ifElse(isTCO)(LibEithers.bind(encodeTermMultilineTCO(cx)(env)(name)(args)(body))(((tcoStmts: ReadonlyArray<PythonSyntax.Statement>) => (() => {
  const trueExpr = ({ tag: "simple", value: PythonUtils.pyAtomToPyExpression(({ tag: "true" })) });
  return (() => {
  const whileBody = PythonUtils.indentedBlock(null)([LibLists.concat2(prefixes)(tcoStmts)]);
  return (() => {
  const whileStmt = ({ tag: "compound", value: ({ tag: "while", value: ({
    condition: trueExpr,
    body: whileBody,
    else: null
  }) }) });
  return ({ tag: "right", value: PythonUtils.indentedBlock(comment)([[whileStmt]]) });
})();
})();
})())))(LibEithers.bind(encodeTermMultiline(cx)(env)(body))(((stmts: ReadonlyArray<PythonSyntax.Statement>) => ({ tag: "right", value: PythonUtils.indentedBlock(comment)([LibLists.concat2(prefixes)(stmts)]) })))))(((block: PythonSyntax.Block) => LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: null }))(((cod: Core.Type) => LibEithers.bind(encodeType(env)(cod))(((pytyp: PythonSyntax.Expression) => ({ tag: "right", value: pytyp })))))(mcod))(((mreturnType: PythonSyntax.Expression | null) => (() => {
  const pyTparams = LibLogic.ifElse(useInlineTypeParams)(LibLists.map(((arg_: Core.Name) => PythonUtils.pyNameToPyTypeParameter(PythonNames.encodeTypeVariable(arg_))))(tparams))([]);
  return (() => {
  const isThunk = LibLists.null_(args);
  return (() => {
  const mDecorators = LibLogic.ifElse(isThunk)([lruCacheDecorator])(null);
  return (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(name);
  return ({ tag: "right", value: ({ tag: "compound", value: ({ tag: "function", value: ({
    decorators: mDecorators,
    raw: ({
    async: false,
    name: pyName,
    typeParams: pyTparams,
    params: pyParams,
    returnType: mreturnType,
    funcTypeComment: null,
    block: block
  })
  }) }) }) });
})();
})();
})();
})()))));
})();
})())))))))))));
}

export function encodeFunctionType<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.FunctionType) => t0 | PythonSyntax.Expression) {
  return ((ft: Core.FunctionType) => (() => {
  const gatherParams = ((rdoms: ReadonlyArray<Core.Type>) => ((ftype: Core.FunctionType) => (() => {
  const innerCod = ((_x) => _x.codomain)(ftype);
  return (() => {
  const dom = ((_x) => _x.domain)(ftype);
  return (() => {
  const _m = Strip.deannotateType(innerCod);
  switch (_m.tag) {
    case "function": return ((ft2: Core.FunctionType) => gatherParams(LibLists.cons(dom)(rdoms))(ft2))((_m as any).value);
    case "annotated": return ((_: Core.AnnotatedType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "application": return ((_: Core.ApplicationType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "forall": return ((_: Core.ForallType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "list": return ((_: Core.Type) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "literal": return ((_: Core.LiteralType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "map": return ((_: Core.MapType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "maybe": return ((_: Core.Type) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "either": return ((_: Core.EitherType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "pair": return ((_: Core.PairType) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "set": return ((_: Core.Type) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "unit": return ((_: void) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "variable": return ((_: Core.Name) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "void": return ((_: void) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
    case "wrap": return ((_: Core.Type) => [LibLists.reverse(LibLists.cons(dom)(rdoms)), innerCod])((_m as any).value);
  }
})();
})();
})()));
  return (() => {
  const domsAndCod = gatherParams([])(ft);
  return (() => {
  const doms = LibPairs.first(domsAndCod);
  return (() => {
  const cod = LibPairs.second(domsAndCod);
  return LibEithers.bind(LibEithers.mapList(((v1: Core.Type) => encodeType(env)(v1)))(doms))(((pydoms: ReadonlyArray<PythonSyntax.Expression>) => LibEithers.bind(encodeType(env)(cod))(((pycod: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithSlices(({ tag: "simple", value: ({ tag: "name", value: "Callable" }) }))(PythonUtils.pyPrimaryToPySlice(({ tag: "simple", value: ({ tag: "list", value: PythonUtils.pyList(pydoms) }) })))([({ tag: "slice", value: PythonUtils.pyExpressionToPySlice(pycod) })])) })))));
})();
})();
})();
})());
}

export function encodeIntegerValue<t0>(iv: Core.IntegerValue): t0 | PythonSyntax.Expression {
  return (() => {
  const toPyInt = ((n: bigint) => ({ tag: "right", value: PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "integer", value: n }) })) }));
  return (() => {
  const _m = iv;
  switch (_m.tag) {
    case "bigint": return ((i: bigint) => toPyInt(i))((_m as any).value);
    case "int8": return ((i: number) => toPyInt(LibLiterals.int8ToBigint(i)))((_m as any).value);
    case "int16": return ((i: bigint) => toPyInt(LibLiterals.int16ToBigint(i)))((_m as any).value);
    case "int32": return ((i: number) => toPyInt(LibLiterals.int32ToBigint(i)))((_m as any).value);
    case "int64": return ((i: bigint) => toPyInt(LibLiterals.int64ToBigint(i)))((_m as any).value);
    case "uint8": return ((i: bigint) => toPyInt(LibLiterals.uint8ToBigint(i)))((_m as any).value);
    case "uint16": return ((i: number) => toPyInt(LibLiterals.uint16ToBigint(i)))((_m as any).value);
    case "uint32": return ((i: bigint) => toPyInt(LibLiterals.uint32ToBigint(i)))((_m as any).value);
    case "uint64": return ((i: bigint) => toPyInt(LibLiterals.uint64ToBigint(i)))((_m as any).value);
  }
})();
})();
}

export function encodeLiteral<t0>(lit: Core.Literal): t0 | PythonSyntax.Expression {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((bs: Uint8Array) => (() => {
  const byteValues = LibLiterals.binaryToBytes(bs);
  return ({ tag: "right", value: PythonUtils.functionCall(({ tag: "simple", value: ({ tag: "name", value: "bytes" }) }))([PythonUtils.pyAtomToPyExpression(({ tag: "list", value: PythonUtils.pyList(LibLists.map(((byteVal: number) => PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "integer", value: LibLiterals.int32ToBigint(byteVal) }) }))))(byteValues)) }))]) });
})())((_m as any).value);
    case "boolean": return ((b: boolean) => ({ tag: "right", value: PythonUtils.pyAtomToPyExpression(LibLogic.ifElse(b)(({ tag: "true" }))(({ tag: "false" }))) }))((_m as any).value);
    case "float": return ((f: Core.FloatValue) => encodeFloatValue(f))((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => encodeIntegerValue(i))((_m as any).value);
    case "string": return ((s: string) => ({ tag: "right", value: PythonUtils.stringToPyExpression(({ tag: "double" }))(s) }))((_m as any).value);
  }
})();
}

export function encodeLiteralType<t0>(lt: Core.LiteralType): t0 | PythonSyntax.Expression {
  return (() => {
  const findName = (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => "bytes")((_m as any).value);
    case "boolean": return ((_: void) => "bool")((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => "Decimal")((_m as any).value);
    case "float32": return ((_: void) => "float")((_m as any).value);
    case "float64": return ((_: void) => "float")((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((_: Core.IntegerType) => "int")((_m as any).value);
    case "string": return ((_: void) => "str")((_m as any).value);
  }
})();
  return ({ tag: "right", value: ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: findName }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }) });
})();
}

export function encodeNameConstants(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ReadonlyArray<PythonSyntax.Statement>)) {
  return ((name: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const toStmt = ((pair: readonly [PythonSyntax.Name, Core.Name]) => PythonUtils.assignmentStatement(LibPairs.first(pair))(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "pascal" }))(env)("hydra.core.Name")))([PythonUtils.doubleQuotedString(((_x) => _x)(LibPairs.second(pair)))])));
  return (() => {
  const namePair = [PythonNames.encodeConstantForTypeName(env)(name), name];
  return (() => {
  const fieldPairs = LibLists.map(((field: Core.FieldType) => [PythonNames.encodeConstantForFieldName(env)(name)(((_x) => _x.name)(field)), ((_x) => _x.name)(field)]))(fields);
  return LibLists.map(toStmt)(LibLists.cons(namePair)(fieldPairs));
})();
})();
})()));
}

export function encodePythonModule(cx: Context.Context): ((x: Graph.Graph) => ((x: Packaging.Module) => ((x: ReadonlyArray<Packaging.Definition>) => Errors.Error | PythonSyntax.Module))) {
  return ((g: Graph.Graph) => ((mod: Packaging.Module) => ((defs0: ReadonlyArray<Packaging.Definition>) => (() => {
  const defs = Environment.reorderDefs(defs0);
  return (() => {
  const meta0 = gatherMetadata(((_x) => _x.namespace)(mod))(defs);
  return (() => {
  const namespaces0 = ((_x) => _x.namespaces)(meta0);
  return (() => {
  const env0 = initialEnvironment(namespaces0)(g);
  return (() => {
  const isTypeMod = isTypeModuleCheck(defs0);
  return withDefinitions(env0)(defs)(((env: PythonEnvironment.PythonEnvironment) => LibEithers.bind(LibEithers.map(((xs: ReadonlyArray<ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>>) => LibLists.concat(xs)))(LibEithers.mapList(((d: Packaging.Definition) => encodeDefinition(cx)(env)(d)))(defs)))(((defStmts: ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>) => (() => {
  const meta2 = LibLogic.ifElse(LibLogic.and(LibLogic.not(isTypeMod))(useInlineTypeParams))(setMetaUsesTypeVar(meta0)(false))(meta0);
  return (() => {
  const meta = LibLogic.ifElse(LibLogic.and(isTypeMod)(LibEquality.equal(targetPythonVersion)(({ tag: "python310" }))))(setMetaUsesTypeAlias(meta2)(true))(meta2);
  return (() => {
  const namespaces = ((_x) => _x.namespaces)(meta0);
  return (() => {
  const commentStmts = LibMaybes.maybe([])(((c: string) => [PythonUtils.commentStatement(c)]))(LibMaybes.map(Formatting.normalizeComment)(((_x) => _x.description)(mod)));
  return (() => {
  const importStmts = moduleImports(namespaces)(meta);
  return (() => {
  const tvars = LibLogic.ifElse(LibLogic.or(isTypeMod)(LibLogic.not(useInlineTypeParams)))(((_x) => _x.typeVariables)(meta))(LibSets.empty);
  return (() => {
  const tvarStmts = LibLists.map(((tv: Core.Name) => tvarStatement(PythonNames.encodeTypeVariable(tv))))(LibSets.toList(tvars));
  return (() => {
  const body = LibLists.filter(((group: ReadonlyArray<PythonSyntax.Statement>) => LibLogic.not(LibLists.null_(group))))(LibLists.concat([[commentStmts, importStmts, tvarStmts], defStmts]));
  return ({ tag: "right", value: body });
})();
})();
})();
})();
})();
})();
})();
})()))));
})();
})();
})();
})();
})())));
}

export function encodeRecordType<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: string | null) => Errors.Error | PythonSyntax.Statement)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((rowType: ReadonlyArray<Core.FieldType>) => ((comment: string | null) => LibEithers.bind(LibEithers.mapList(((v1: Core.FieldType) => encodeFieldType(cx)(env)(v1)))(rowType))(((pyFields: ReadonlyArray<PythonSyntax.Statement>) => (() => {
  const constStmts = encodeNameConstants(env)(name)(rowType);
  return (() => {
  const body = PythonUtils.indentedBlock(comment)([pyFields, constStmts]);
  return (() => {
  const boundVars = ((_x) => _x.boundTypeVariables)(env);
  return (() => {
  const tparamList = LibPairs.first(boundVars);
  return (() => {
  const mGenericArg = genericArg(tparamList);
  return (() => {
  const args = LibMaybes.maybe(null)(((a: PythonSyntax.Expression) => PythonUtils.pyExpressionsToPyArgs([a])))(mGenericArg);
  return (() => {
  const decs = [dataclassDecorator];
  return (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "pascal" }))(env)(name);
  return (() => {
  const noTypeParams = [];
  return ({ tag: "right", value: PythonUtils.pyClassDefinitionToPyStatement(({
    decorators: decs,
    name: pyName,
    typeParams: noTypeParams,
    arguments: args,
    body: body
  })) });
})();
})();
})();
})();
})();
})();
})();
})();
})()))))));
}

export function encodeTermAssignment(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.Term) => ((x: Core.TypeScheme) => ((x: string | null) => Errors.Error | PythonSyntax.Statement))))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((term: Core.Term) => ((ts: Core.TypeScheme) => ((comment: string | null) => LibEithers.bind(analyzePythonFunction(cx)(env)(term))(((fs: Typing.FunctionStructure<PythonEnvironment.PythonEnvironment>) => (() => {
  const tparams = ((_x) => _x.typeParams)(fs);
  return (() => {
  const params = ((_x) => _x.params)(fs);
  return (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const body = ((_x) => _x.body)(fs);
  return (() => {
  const doms = ((_x) => _x.domains)(fs);
  return (() => {
  const mcod = ((_x) => _x.codomain)(fs);
  return (() => {
  const env2 = ((_x) => _x.environment)(fs);
  return (() => {
  const tc = ((_x) => _x.graph)(env2);
  return (() => {
  const binding = ({
    name: name,
    term: term,
    type: ts
  });
  return (() => {
  const isComplex = Predicates.isComplexBinding(tc)(binding);
  return (() => {
  const isTrivial = Predicates.isTrivialTerm(term);
  return LibLogic.ifElse(LibLogic.and(isComplex)(LibLogic.not(isTrivial)))(LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeBindingAs(cx)(env2)(v1)))(bindings))(((bindingStmts: ReadonlyArray<PythonSyntax.Statement>) => encodeFunctionDefinition(cx)(env2)(name)(tparams)(params)(body)(doms)(mcod)(comment)(bindingStmts))))(LibEithers.bind(encodeTermInline(cx)(env2)(false)(body))(((bodyExpr: PythonSyntax.Expression) => (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env2)(name);
  return ({ tag: "right", value: PythonUtils.annotatedStatement(comment)(PythonUtils.assignmentStatement(pyName)(bodyExpr)) });
})())));
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
})())))))));
}

export function encodeTermInline(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: boolean) => ((x: Core.Term) => Errors.Error | PythonSyntax.Expression))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((noCast: boolean) => ((term: Core.Term) => (() => {
  const encode = ((t: Core.Term) => encodeTermInline(cx)(env)(false)(t));
  return (() => {
  const stripTypeApps = ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((ann: Core.AnnotatedTerm) => stripTypeApps(((_x) => _x.body)(ann)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => stripTypeApps(((_x) => _x.body)(ta)))((_m as any).value);
    default: return t(_m);
  }
})());
  return (() => {
  const withCast = ((pyexp: PythonSyntax.Expression) => LibLogic.ifElse(LibLogic.or(noCast)(((_x) => _x.skipCasts)(env)))(({ tag: "right", value: pyexp }))((() => {
  const tc = ((_x) => _x.graph)(env);
  return (() => {
  const mtyp = LibEithers.map(((_r: readonly [Core.Type, Context.Context]) => LibPairs.first(_r)))(Checking.typeOf(cx)(tc)([])(term));
  return LibEithers.either(((_: Errors.Error) => ({ tag: "right", value: pyexp })))(((typ: Core.Type) => LibEithers.either(((_: void) => ({ tag: "right", value: pyexp })))(((pytyp: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.castTo(pytyp)(pyexp) })))(encodeType(env)(typ))))(mtyp);
})();
})()));
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(term);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => encodeApplication(cx)(env)(app))((_m as any).value);
    case "either": return ((et: Core.Term | Core.Term) => LibEithers.either(((t1: Core.Term) => LibEithers.bind(encode(t1))(((pyexp: PythonSyntax.Expression) => withCast(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("Left"))([pyexp]))))))(((t1: Core.Term) => LibEithers.bind(encode(t1))(((pyexp: PythonSyntax.Expression) => withCast(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("Right"))([pyexp]))))))(et))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => LibEithers.bind(analyzePythonFunction(cx)(env)(({ tag: "lambda", value: lam })))(((fs: Typing.FunctionStructure<PythonEnvironment.PythonEnvironment>) => (() => {
  const params = ((_x) => _x.params)(fs);
  return (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const innerBody = ((_x) => _x.body)(fs);
  return (() => {
  const innerEnv0 = ((_x) => _x.environment)(fs);
  return (() => {
  const bindingNames = LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(bindings);
  return (() => {
  const innerEnv = ({
    namespaces: ((_x) => _x.namespaces)(innerEnv0),
    boundTypeVariables: ((_x) => _x.boundTypeVariables)(innerEnv0),
    graph: ((_x) => _x.graph)(innerEnv0),
    nullaryBindings: ((_x) => _x.nullaryBindings)(innerEnv0),
    version: ((_x) => _x.version)(innerEnv0),
    skipCasts: ((_x) => _x.skipCasts)(innerEnv0),
    inlineVariables: LibSets.union(LibSets.fromList(bindingNames))(((_x) => _x.inlineVariables)(innerEnv0))
  });
  return LibEithers.bind(encodeTermInline(cx)(innerEnv)(false)(innerBody))(((pbody: PythonSyntax.Expression) => (() => {
  const pparams = LibLists.map(((v1: Core.Name) => PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(innerEnv)(v1)))(params);
  return LibLogic.ifElse(LibLists.null_(bindings))(({ tag: "right", value: makeUncurriedLambda(pparams)(pbody) }))(LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeBindingAsAssignment(cx)(false)(innerEnv)(v1)))(bindings))(((pbindingExprs: ReadonlyArray<PythonSyntax.NamedExpression>) => (() => {
  const pbindingStarExprs = LibLists.map(((ne: PythonSyntax.NamedExpression) => ({ tag: "simple", value: ne })))(pbindingExprs);
  return (() => {
  const pbodyStarExpr = PythonUtils.pyExpressionToPyStarNamedExpression(pbody);
  return (() => {
  const tupleElements = LibLists.concat2(pbindingStarExprs)([pbodyStarExpr]);
  return (() => {
  const tupleExpr = PythonUtils.pyAtomToPyExpression(({ tag: "tuple", value: tupleElements }));
  return (() => {
  const indexValue = PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "integer", value: LibLiterals.int32ToBigint(LibLists.length(bindings)) }) }));
  return (() => {
  const indexedExpr = PythonUtils.primaryWithExpressionSlices(PythonUtils.pyExpressionToPyPrimary(tupleExpr))([indexValue]);
  return ({ tag: "right", value: makeUncurriedLambda(pparams)(PythonUtils.pyPrimaryToPyExpression(indexedExpr)) });
})();
})();
})();
})();
})();
})())));
})()));
})();
})();
})();
})();
})();
})())))((_m as any).value);
    case "project": return ((proj: Core.Projection) => (() => {
  const fname = ((_x) => _x.field)(proj);
  return ({ tag: "right", value: makeCurriedLambda(["v1"])(PythonUtils.projectFromExpression(({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "v1" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }))(PythonNames.encodeFieldName(env)(fname))) });
})())((_m as any).value);
    case "unwrap": return ((_: Core.Name) => ({ tag: "right", value: makeCurriedLambda(["v1"])(PythonUtils.projectFromExpression(({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "v1" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }))("value")) }))((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => ({ tag: "right", value: unsupportedExpression("case expressions as values are not yet supported") }))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return LibLogic.ifElse(LibLists.null_(bindings))(encodeTermInline(cx)(env)(false)(body))(withLetInline(env)(lt)(((innerEnv: PythonEnvironment.PythonEnvironment) => LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeBindingAsAssignment(cx)(false)(innerEnv)(v1)))(bindings))(((pbindingExprs: ReadonlyArray<PythonSyntax.NamedExpression>) => LibEithers.bind(encodeTermInline(cx)(innerEnv)(false)(body))(((pbody: PythonSyntax.Expression) => (() => {
  const pbindingStarExprs = LibLists.map(((ne: PythonSyntax.NamedExpression) => ({ tag: "simple", value: ne })))(pbindingExprs);
  return (() => {
  const pbodyStarExpr = PythonUtils.pyExpressionToPyStarNamedExpression(pbody);
  return (() => {
  const tupleElements = LibLists.concat2(pbindingStarExprs)([pbodyStarExpr]);
  return (() => {
  const tupleExpr = PythonUtils.pyAtomToPyExpression(({ tag: "tuple", value: tupleElements }));
  return (() => {
  const indexValue = PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "integer", value: LibLiterals.int32ToBigint(LibLists.length(bindings)) }) }));
  return (() => {
  const indexedExpr = PythonUtils.primaryWithExpressionSlices(PythonUtils.pyExpressionToPyPrimary(tupleExpr))([indexValue]);
  return ({ tag: "right", value: PythonUtils.pyPrimaryToPyExpression(indexedExpr) });
})();
})();
})();
})();
})();
})())))))));
})();
})())((_m as any).value);
    case "list": return ((terms: ReadonlyArray<Core.Term>) => LibEithers.bind(LibEithers.mapList(encode)(terms))(((pyExprs: ReadonlyArray<PythonSyntax.Expression>) => ({ tag: "right", value: PythonUtils.pyAtomToPyExpression(({ tag: "tuple", value: LibLists.map(PythonUtils.pyExpressionToPyStarNamedExpression)(pyExprs) })) }))))((_m as any).value);
    case "literal": return ((lit: Core.Literal) => encodeLiteral(lit))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibEithers.bind(LibEithers.mapList(((kv: readonly [Core.Term, Core.Term]) => (() => {
  const k = LibPairs.first(kv);
  return (() => {
  const v = LibPairs.second(kv);
  return LibEithers.bind(encode(k))(((pyK: PythonSyntax.Expression) => LibEithers.bind(encode(v))(((pyV: PythonSyntax.Expression) => ({ tag: "right", value: ({ tag: "pair", value: ({
    key: pyK,
    value: pyV
  }) }) })))));
})();
})()))(LibMaps.toList(m)))(((pairs: ReadonlyArray<PythonSyntax.DoubleStarredKvpair>) => ({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("FrozenDict"))([PythonUtils.pyAtomToPyExpression(({ tag: "dict", value: pairs }))]) }))))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.maybe(({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("Nothing"))([]) }))(((t1: Core.Term) => LibEithers.bind(encode(t1))(((pyexp: PythonSyntax.Expression) => withCast(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("Just"))([pyexp]))))))(mt))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => (() => {
  const t1 = LibPairs.first(p);
  return (() => {
  const t2 = LibPairs.second(p);
  return LibEithers.bind(encode(t1))(((pyExpr1: PythonSyntax.Expression) => LibEithers.bind(encode(t2))(((pyExpr2: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.pyAtomToPyExpression(({ tag: "tuple", value: [PythonUtils.pyExpressionToPyStarNamedExpression(pyExpr1), PythonUtils.pyExpressionToPyStarNamedExpression(pyExpr2)] })) })))));
})();
})())((_m as any).value);
    case "record": return ((r: Core.Record) => (() => {
  const tname = ((_x) => _x.typeName)(r);
  return (() => {
  const fields = ((_x) => _x.fields)(r);
  return LibEithers.bind(LibEithers.mapList(((fld: Core.Field) => encode(((_x) => _x.term)(fld))))(fields))(((pargs: ReadonlyArray<PythonSyntax.Expression>) => ({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeNameQualified(env)(tname)))(pargs) })));
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibEithers.bind(LibEithers.mapList(encode)(LibSets.toList(s)))(((pyEls: ReadonlyArray<PythonSyntax.Expression>) => ({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary("frozenset"))([PythonUtils.pyAtomToPyExpression(({ tag: "set", value: LibLists.map(PythonUtils.pyExpressionToPyStarNamedExpression)(pyEls) }))]) }))))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const body = ((_x) => _x.body)(ta);
  return LibEithers.bind(encodeTermInline(cx)(env)(true)(stripTypeApps(body)))(((pybase: PythonSyntax.Expression) => withCast(pybase)));
})())((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => (() => {
  const body = ((_x) => _x.body)(tl);
  return withTypeLambda(env)(tl)(((env2: PythonEnvironment.PythonEnvironment) => encodeTermInline(cx)(env2)(noCast)(body)));
})())((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const tname = ((_x) => _x.typeName)(inj);
  return (() => {
  const field = ((_x) => _x.field)(inj);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(Predicates.isEnumRowType(rt))(({ tag: "right", value: PythonUtils.projectFromExpression(PythonUtils.pyNameToPyExpression(PythonNames.encodeNameQualified(env)(tname)))(PythonNames.encodeEnumValue(env)(((_x) => _x.name)(field))) }))((() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const isUnitVariant = LibMaybes.maybe(false)(((ft: Core.FieldType) => Predicates.isUnitType(Strip.deannotateType(((_x) => _x.type)(ft)))))(LibLists.find(((ft: Core.FieldType) => LibEquality.equal(((_x) => _x)(((_x) => _x.name)(ft)))(((_x) => _x)(fname))))(rt));
  return LibEithers.bind(LibLogic.ifElse(LibLogic.or(Predicates.isUnitTerm(((_x) => _x.term)(field)))(isUnitVariant))(({ tag: "right", value: [] }))(LibEithers.bind(encode(((_x) => _x.term)(field)))(((parg: PythonSyntax.Expression) => ({ tag: "right", value: [parg] })))))(((args: ReadonlyArray<PythonSyntax.Expression>) => (() => {
  const deconflictedName = deconflictVariantName(true)(env)(tname)(fname)(((_x) => _x.graph)(env));
  return ({ tag: "right", value: PythonUtils.castTo(PythonNames.typeVariableReference(env)(tname))(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(deconflictedName))(args)) });
})()));
})();
})())));
})();
})())((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: PythonUtils.pyNameToPyExpression(PythonUtils.pyNone) }))((_m as any).value);
    case "variable": return ((name: Core.Name) => encodeVariable(cx)(env)(name)([]))((_m as any).value);
    case "wrap": return ((wrapped: Core.WrappedTerm) => (() => {
  const tname = ((_x) => _x.typeName)(wrapped);
  return (() => {
  const inner = ((_x) => _x.body)(wrapped);
  return LibEithers.bind(encode(inner))(((parg: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeNameQualified(env)(tname)))([parg]) })));
})();
})())((_m as any).value);
  }
})();
})();
})();
})())));
}

export function encodeTermMultiline(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PythonSyntax.Statement>)) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((term: Core.Term) => (() => {
  const dfltLogic = LibEithers.bind(analyzePythonFunction(cx)(env)(term))(((fs: Typing.FunctionStructure<PythonEnvironment.PythonEnvironment>) => (() => {
  const params = ((_x) => _x.params)(fs);
  return (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const innerBody = ((_x) => _x.body)(fs);
  return (() => {
  const env2 = ((_x) => _x.environment)(fs);
  return LibLogic.ifElse(LibLists.null_(bindings))(LibEithers.bind(encodeTermInline(cx)(env)(false)(term))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [PythonUtils.returnSingle(expr)] }))))(LibEithers.bind(LibEithers.mapList(((v1: Core.Binding) => encodeBindingAs(cx)(env2)(v1)))(bindings))(((bindingStmts: ReadonlyArray<PythonSyntax.Statement>) => LibEithers.bind(encodeTermMultiline(cx)(env2)(innerBody))(((bodyStmts: ReadonlyArray<PythonSyntax.Statement>) => ({ tag: "right", value: LibLists.concat2(bindingStmts)(bodyStmts) }))))));
})();
})();
})();
})()));
  return (() => {
  const gathered = Analysis.gatherApplications(term);
  return (() => {
  const args = LibPairs.first(gathered);
  return (() => {
  const body = LibPairs.second(gathered);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(args))(1))((() => {
  const arg = LibLists.head(args);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(body);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const isFull = isCasesFull(rt)(cases_);
  return LibEithers.bind(encodeTermInline(cx)(env)(false)(arg))(((pyArg: PythonSyntax.Expression) => LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)(((e: PythonEnvironment.PythonEnvironment) => ((t: Core.Term) => encodeTermMultiline(cx)(e)(t))))(v1)))(deduplicateCaseVariables(cases_)))(((pyCases: ReadonlyArray<PythonSyntax.CaseBlock>) => LibEithers.bind(encodeDefaultCaseBlock(((t: Core.Term) => encodeTermInline(cx)(env)(false)(t)))(isFull)(dflt)(tname))(((pyDflt: ReadonlyArray<PythonSyntax.CaseBlock>) => (() => {
  const subj = ({ tag: "simple", value: ({ tag: "simple", value: pyArg }) });
  return (() => {
  const matchStmt = ({ tag: "compound", value: ({ tag: "match", value: ({
    subject: subj,
    cases: LibLists.concat2(pyCases)(pyDflt)
  }) }) });
  return ({ tag: "right", value: [matchStmt] });
})();
})()))))));
})();
})()));
})();
})();
})())((_m as any).value);
    default: return dfltLogic(_m);
  }
})();
})())(dfltLogic);
})();
})();
})();
})()));
}

export function encodeTermMultilineTCO(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<Core.Name>) => ((x: Core.Term) => Errors.Error | ReadonlyArray<PythonSyntax.Statement>)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((funcName: Core.Name) => ((paramNames: ReadonlyArray<Core.Name>) => ((term: Core.Term) => (() => {
  const stripped = Strip.deannotateAndDetypeTerm(term);
  return (() => {
  const gathered = Analysis.gatherApplications(stripped);
  return (() => {
  const gatherArgs = LibPairs.first(gathered);
  return (() => {
  const gatherFun = LibPairs.second(gathered);
  return (() => {
  const strippedFun = Strip.deannotateAndDetypeTerm(gatherFun);
  return (() => {
  const isSelfCall = (() => {
  const _m = strippedFun;
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => LibEquality.equal(n)(funcName))((_m as any).value);
    default: return false(_m);
  }
})();
  return LibLogic.ifElse(LibLogic.and(isSelfCall)(LibEquality.equal(LibLists.length(gatherArgs))(LibLists.length(paramNames))))(LibEithers.bind(LibEithers.mapList(((a: Core.Term) => encodeTermInline(cx)(env)(false)(a)))(gatherArgs))(((pyArgs: ReadonlyArray<PythonSyntax.Expression>) => (() => {
  const assignments = LibLists.map(((pair: readonly [Core.Name, PythonSyntax.Expression]) => (() => {
  const paramName = LibPairs.first(pair);
  return (() => {
  const pyArg = LibPairs.second(pair);
  return PythonUtils.assignmentStatement(PythonNames.encodeName(false)(({ tag: "lowerSnake" }))(env)(paramName))(pyArg);
})();
})()))(LibLists.zip(paramNames)(pyArgs));
  return (() => {
  const continueStmt = ({ tag: "simple", value: [({ tag: "continue" })] });
  return ({ tag: "right", value: LibLists.concat2(assignments)([continueStmt]) });
})();
})())))((() => {
  const gathered2 = Analysis.gatherApplications(term);
  return (() => {
  const args2 = LibPairs.first(gathered2);
  return (() => {
  const body2 = LibPairs.second(gathered2);
  return LibLogic.ifElse(LibEquality.equal(LibLists.length(args2))(1))((() => {
  const arg = LibLists.head(args2);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(body2);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const isFull = isCasesFull(rt)(cases_);
  return LibEithers.bind(encodeTermInline(cx)(env)(false)(arg))(((pyArg: PythonSyntax.Expression) => LibEithers.bind(LibEithers.mapList(((v1: Core.Field) => encodeCaseBlock(cx)(env)(tname)(rt)(isEnum)(((e2: PythonEnvironment.PythonEnvironment) => ((t2: Core.Term) => encodeTermMultilineTCO(cx)(e2)(funcName)(paramNames)(t2))))(v1)))(deduplicateCaseVariables(cases_)))(((pyCases: ReadonlyArray<PythonSyntax.CaseBlock>) => LibEithers.bind(encodeDefaultCaseBlock(((t2: Core.Term) => encodeTermInline(cx)(env)(false)(t2)))(isFull)(dflt)(tname))(((pyDflt: ReadonlyArray<PythonSyntax.CaseBlock>) => (() => {
  const subj = ({ tag: "simple", value: ({ tag: "simple", value: pyArg }) });
  return (() => {
  const matchStmt = ({ tag: "compound", value: ({ tag: "match", value: ({
    subject: subj,
    cases: LibLists.concat2(pyCases)(pyDflt)
  }) }) });
  return ({ tag: "right", value: [matchStmt] });
})();
})()))))));
})();
})()));
})();
})();
})())((_m as any).value);
    default: return LibEithers.bind(encodeTermInline(cx)(env)(false)(term))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [PythonUtils.returnSingle(expr)] })))(_m);
  }
})();
})())(LibEithers.bind(encodeTermInline(cx)(env)(false)(term))(((expr: PythonSyntax.Expression) => ({ tag: "right", value: [PythonUtils.returnSingle(expr)] }))));
})();
})();
})());
})();
})();
})();
})();
})();
})()))));
}

export function encodeType<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.Type) => t0 | PythonSyntax.Expression) {
  return ((typ: Core.Type) => (() => {
  const dflt = ({ tag: "right", value: PythonUtils.doubleQuotedString(LibStrings.cat2("type = ")(ShowCore.type(Strip.deannotateType(typ)))) });
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "application": return ((at: Core.ApplicationType) => encodeApplicationType(env)(at))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => encodeFunctionType(env)(ft))((_m as any).value);
    case "forall": return ((lt: Core.ForallType) => encodeForallType(env)(lt))((_m as any).value);
    case "list": return ((et: Core.Type) => LibEithers.bind(encodeType(env)(et))(((pyet: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.nameAndParams("frozenlist")([pyet]) }))))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibEithers.bind(encodeType(env)(((_x) => _x.keys)(mt)))(((pykt: PythonSyntax.Expression) => LibEithers.bind(encodeType(env)(((_x) => _x.values)(mt)))(((pyvt: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.nameAndParams("FrozenDict")([pykt, pyvt]) }))))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => encodeLiteralType(lt))((_m as any).value);
    case "maybe": return ((et: Core.Type) => LibEithers.bind(encodeType(env)(et))(((ptype: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithExpressionSlices(({ tag: "simple", value: ({ tag: "name", value: "Maybe" }) }))([ptype])) }))))((_m as any).value);
    case "either": return ((eitherT: Core.EitherType) => LibEithers.bind(encodeType(env)(((_x) => _x.left)(eitherT)))(((pyleft: PythonSyntax.Expression) => LibEithers.bind(encodeType(env)(((_x) => _x.right)(eitherT)))(((pyright: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithExpressionSlices(({ tag: "simple", value: ({ tag: "name", value: "Either" }) }))([pyleft, pyright])) }))))))((_m as any).value);
    case "pair": return ((pairT: Core.PairType) => LibEithers.bind(encodeType(env)(((_x) => _x.first)(pairT)))(((pyFirst: PythonSyntax.Expression) => LibEithers.bind(encodeType(env)(((_x) => _x.second)(pairT)))(((pySecond: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.nameAndParams("tuple")([pyFirst, pySecond]) }))))))((_m as any).value);
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => dflt)((_m as any).value);
    case "set": return ((et: Core.Type) => LibEithers.bind(encodeType(env)(et))(((pyet: PythonSyntax.Expression) => ({ tag: "right", value: PythonUtils.nameAndParams("frozenset")([pyet]) }))))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => dflt)((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: PythonUtils.pyNameToPyExpression(PythonUtils.pyNone) }))((_m as any).value);
    case "void": return ((_: void) => ({ tag: "right", value: PythonUtils.pyNameToPyExpression(PythonUtils.pyNone) }))((_m as any).value);
    case "variable": return ((name: Core.Name) => ({ tag: "right", value: PythonNames.typeVariableReference(env)(name) }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => dflt)((_m as any).value);
    case "annotated": return ((_: Core.AnnotatedType) => dflt)((_m as any).value);
  }
})();
})());
}

export function encodeTypeAssignment<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.Type) => ((x: string | null) => Errors.Error | ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((typ: Core.Type) => ((comment: string | null) => LibEithers.bind(encodeTypeAssignmentInner(cx)(env)(name)(typ)(comment))(((defStmts: ReadonlyArray<PythonSyntax.Statement>) => ({ tag: "right", value: LibLists.map(((s: PythonSyntax.Statement) => [s]))(defStmts) })))))));
}

export function encodeTypeAssignmentInner<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.Type) => ((x: string | null) => Errors.Error | ReadonlyArray<PythonSyntax.Statement>)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((typ: Core.Type) => ((comment: string | null) => (() => {
  const stripped = Strip.deannotateType(typ);
  return (() => {
  const dflt = LibEithers.bind(encodeType(env)(typ))(((typeExpr: PythonSyntax.Expression) => ({ tag: "right", value: encodeTypeDefSingle(env)(name)(comment)(typeExpr) })));
  return (() => {
  const _m = stripped;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => (() => {
  const tvar = ((_x) => _x.parameter)(ft);
  return (() => {
  const body = ((_x) => _x.body)(ft);
  return (() => {
  const newEnv = extendEnvWithTypeVar(env)(tvar);
  return encodeTypeAssignmentInner(cx)(newEnv)(name)(body)(comment);
})();
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibEithers.map(((s: PythonSyntax.Statement) => [s]))(encodeRecordType(cx)(env)(name)(rt)(comment)))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => encodeUnionType(cx)(env)(name)(rt)(comment))((_m as any).value);
    case "wrap": return ((wt: Core.Type) => encodeWrappedType(env)(name)(wt)(comment))((_m as any).value);
    default: return dflt(_m);
  }
})();
})();
})()))));
}

export function encodeTypeDefSingle(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: string | null) => ((x: PythonSyntax.Expression) => ReadonlyArray<PythonSyntax.Statement>))) {
  return ((name: Core.Name) => ((comment: string | null) => ((typeExpr: PythonSyntax.Expression) => (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "pascal" }))(env)(name);
  return (() => {
  const tparams = environmentTypeParameters(env);
  return [typeAliasStatementFor(env)(pyName)(tparams)(comment)(typeExpr)];
})();
})())));
}

export function encodeTypeQuoted<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.Type) => t0 | PythonSyntax.Expression) {
  return ((typ: Core.Type) => LibEithers.bind(encodeType(env)(typ))(((pytype: PythonSyntax.Expression) => ({ tag: "right", value: LibLogic.ifElse(LibSets.null_(Variables.freeVariablesInType(typ)))(pytype)(PythonUtils.doubleQuotedString(Serialization.printExpr(PythonSerde.encodeExpression(pytype)))) }))));
}

export function encodeUnionEliminationInline(cx: Context.Context): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.CaseStatement) => ((x: PythonSyntax.Expression) => Errors.Error | PythonSyntax.Expression))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((cs: Core.CaseStatement) => ((pyArg: PythonSyntax.Expression) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const mdefault = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(Resolution.requireUnionType(cx)(pythonEnvironmentGetGraph(env))(tname))(((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const isEnum = Predicates.isEnumRowType(rt);
  return (() => {
  const valueExpr = PythonUtils.projectFromExpression(pyArg)("value");
  return (() => {
  const isinstancePrimary = PythonUtils.pyNameToPyPrimary("isinstance");
  return LibEithers.bind(LibMaybes.maybe(({ tag: "right", value: unsupportedExpression("no matching case in inline union elimination") }))(((dflt: Core.Term) => encodeTermInline(cx)(env)(false)(dflt)))(mdefault))(((pyDefault: PythonSyntax.Expression) => (() => {
  const encodeBranch = ((field: Core.Field) => (() => {
  const fname = ((_x) => _x.name)(field);
  return (() => {
  const fterm = ((_x) => _x.term)(field);
  return (() => {
  const isUnitVariant = isVariantUnitType(rt)(fname);
  return (() => {
  const pyVariantName = deconflictVariantName(true)(env)(tname)(fname)(((_x) => _x.graph)(env));
  return (() => {
  const isinstanceCheck = LibLogic.ifElse(isEnum)(({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: PythonUtils.pyExpressionToBitwiseOr(pyArg),
    rhs: [({
    operator: ({ tag: "eq" }),
    rhs: PythonUtils.pyExpressionToBitwiseOr(PythonUtils.pyNameToPyExpression(pyVariantName))
  })]
  }) })]] }))(PythonUtils.functionCall(isinstancePrimary)([pyArg, PythonUtils.pyNameToPyExpression(pyVariantName)]));
  return LibEithers.bind(encodeTermInline(cx)(env)(false)(fterm))(((pyBranch: PythonSyntax.Expression) => (() => {
  const pyResult = LibLogic.ifElse(isEnum)(PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(pyBranch))([pyArg]))(LibLogic.ifElse(isUnitVariant)(PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(pyBranch))([pyArg]))(PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(pyBranch))([valueExpr])));
  return ({ tag: "right", value: [isinstanceCheck, pyResult] });
})()));
})();
})();
})();
})();
})());
  return LibEithers.bind(LibEithers.mapList(encodeBranch)(cases_))(((encodedBranches: ReadonlyArray<readonly [PythonSyntax.Expression, PythonSyntax.Expression]>) => (() => {
  const buildChain = ((elseExpr: PythonSyntax.Expression) => ((branchPair: readonly [PythonSyntax.Expression, PythonSyntax.Expression]) => (() => {
  const checkExpr = LibPairs.first(branchPair);
  return (() => {
  const resultExpr = LibPairs.second(branchPair);
  return ({ tag: "conditional", value: ({
    body: PythonUtils.pyExpressionToDisjunction(resultExpr),
    if: PythonUtils.pyExpressionToDisjunction(checkExpr),
    else: elseExpr
  }) });
})();
})()));
  return ({ tag: "right", value: LibLists.foldl(buildChain)(pyDefault)(LibLists.reverse(encodedBranches)) });
})()));
})()));
})();
})();
})()));
})();
})();
})())));
}

export function encodeUnionField<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: Core.FieldType) => Errors.Error | PythonSyntax.Statement))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((unionName: Core.Name) => ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  return (() => {
  const ftype = ((_x) => _x.type)(fieldType);
  return LibEithers.bind(Annotations.getTypeDescription(cx)(pythonEnvironmentGetGraph(env))(ftype))(((fcomment: string | null) => (() => {
  const isUnit = LibEquality.equal(Strip.deannotateType(ftype))(({ tag: "unit" }));
  return (() => {
  const varName = deconflictVariantName(false)(env)(unionName)(fname)(((_x) => _x.graph)(env));
  return (() => {
  const tparamNames = findTypeParams(env)(ftype);
  return (() => {
  const tparamPyNames = LibLists.map(PythonNames.encodeTypeVariable)(tparamNames);
  return (() => {
  const fieldParams = LibLists.map(PythonUtils.pyNameToPyTypeParameter)(tparamPyNames);
  return (() => {
  const body = LibLogic.ifElse(isUnit)(PythonUtils.indentedBlock(fcomment)([PythonUtils.unitVariantMethods(varName)]))(PythonUtils.indentedBlock(fcomment)([]));
  return LibEithers.bind(LibLogic.ifElse(isUnit)(({ tag: "right", value: null }))(LibEithers.bind(encodeTypeQuoted(env)(ftype))(((quotedType: PythonSyntax.Expression) => ({ tag: "right", value: variantArgs(quotedType)([]) })))))(((margs: PythonSyntax.Args | null) => ({ tag: "right", value: PythonUtils.pyClassDefinitionToPyStatement(({
    decorators: null,
    name: varName,
    typeParams: fieldParams,
    arguments: margs,
    body: body
  })) })));
})();
})();
})();
})();
})();
})()));
})();
})())));
}

export function encodeUnionFieldAlt(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: Core.FieldType) => PythonSyntax.Primary)) {
  return ((unionName: Core.Name) => ((fieldType: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(fieldType);
  return (() => {
  const ftype = ((_x) => _x.type)(fieldType);
  return (() => {
  const tparamNames = findTypeParams(env)(ftype);
  return (() => {
  const tparams = LibLists.map(PythonNames.encodeTypeVariable)(tparamNames);
  return (() => {
  const namePrim = PythonUtils.pyNameToPyPrimary(PythonNames.variantName(false)(env)(unionName)(fname));
  return LibLogic.ifElse(LibLists.null_(tparams))(namePrim)((() => {
  const tparamExprs = LibLists.map(PythonUtils.pyNameToPyExpression)(tparams);
  return PythonUtils.primaryWithExpressionSlices(namePrim)(tparamExprs);
})());
})();
})();
})();
})();
})()));
}

export function encodeUnionType<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: string | null) => Errors.Error | ReadonlyArray<PythonSyntax.Statement>)))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((rowType: ReadonlyArray<Core.FieldType>) => ((comment: string | null) => LibLogic.ifElse(Predicates.isEnumRowType(rowType))(LibEithers.bind(LibEithers.mapList(((v1: Core.FieldType) => encodeEnumValueAssignment(cx)(env)(v1)))(rowType))(((vals: ReadonlyArray<ReadonlyArray<PythonSyntax.Statement>>) => (() => {
  const body = PythonUtils.indentedBlock(comment)(vals);
  return (() => {
  const enumName = "Enum";
  return (() => {
  const args = PythonUtils.pyExpressionsToPyArgs([PythonUtils.pyNameToPyExpression(enumName)]);
  return (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "pascal" }))(env)(name);
  return (() => {
  const typeConstStmt = PythonUtils.dottedAssignmentStatement(pyName)(PythonNames.encodeConstantForTypeName(env)(name))(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "pascal" }))(env)("hydra.core.Name")))([PythonUtils.doubleQuotedString(((_x) => _x)(name))]));
  return ({ tag: "right", value: [PythonUtils.pyClassDefinitionToPyStatement(({
    decorators: null,
    name: pyName,
    typeParams: [],
    arguments: args,
    body: body
  })), typeConstStmt] });
})();
})();
})();
})();
})())))((() => {
  const constStmts = encodeNameConstants(env)(name)(rowType);
  return LibEithers.bind(LibEithers.mapList(((v1: Core.FieldType) => encodeUnionField(cx)(env)(name)(v1)))(rowType))(((fieldStmts: ReadonlyArray<PythonSyntax.Statement>) => (() => {
  const tparams = environmentTypeParameters(env);
  return (() => {
  const unionAlts = LibLists.map(((v1: Core.FieldType) => encodeUnionFieldAlt(env)(name)(v1)))(rowType);
  return (() => {
  const unionStmts = unionTypeStatementsFor(env)(PythonNames.encodeName(false)(({ tag: "pascal" }))(env)(name))(tparams)(comment)(PythonUtils.orExpression(unionAlts))(constStmts);
  return ({ tag: "right", value: LibLists.concat2(fieldStmts)(unionStmts) });
})();
})();
})()));
})())))));
}

export function encodeVariable<t0>(cx: t0): ((x: PythonEnvironment.PythonEnvironment) => ((x: Core.Name) => ((x: ReadonlyArray<PythonSyntax.Expression>) => Errors.Error | PythonSyntax.Expression))) {
  return ((env: PythonEnvironment.PythonEnvironment) => ((name: Core.Name) => ((args: ReadonlyArray<PythonSyntax.Expression>) => (() => {
  const g = pythonEnvironmentGetGraph(env);
  return (() => {
  const tc = ((_x) => _x.graph)(env);
  return (() => {
  const tcTypes = ((_x) => _x.boundTypes)(tc);
  return (() => {
  const tcLambdaVars = ((_x) => _x.lambdaVariables)(tc);
  return (() => {
  const tcMetadata = ((_x) => _x.metadata)(tc);
  return (() => {
  const inlineVars = ((_x) => _x.inlineVariables)(env);
  return (() => {
  const mTypScheme = LibMaps.lookup(name)(tcTypes);
  return (() => {
  const mTyp = LibMaybes.map(((ts_: Core.TypeScheme) => ((_x) => _x.type)(ts_)))(mTypScheme);
  return (() => {
  const asVariable = PythonNames.termVariableReference(env)(name);
  return (() => {
  const asFunctionCall = PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "lowerSnake" }))(env)(name)))(args);
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(args)))(LibMaybes.maybe(({ tag: "right", value: asFunctionCall }))(((prim: Graph.Primitive) => (() => {
  const primArity = Arity.primitiveArity(prim);
  return LibLogic.ifElse(LibEquality.equal(primArity)(LibLists.length(args)))(({ tag: "right", value: asFunctionCall }))((() => {
  const numRemaining = LibMath.sub(primArity)(LibLists.length(args));
  return (() => {
  const remainingParams = LibLists.map(((i: number) => LibStrings.cat2("x")(LibLiterals.showInt32(i))))(LibMath.range(1)(numRemaining));
  return (() => {
  const remainingExprs = LibLists.map(((n: PythonSyntax.Name) => ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: n }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })))(remainingParams);
  return (() => {
  const allArgs = LibLists.concat2(args)(remainingExprs);
  return (() => {
  const fullCall = PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "lowerSnake" }))(env)(name)))(allArgs);
  return ({ tag: "right", value: makeUncurriedLambda(remainingParams)(fullCall) });
})();
})();
})();
})();
})());
})()))(Lexical.lookupPrimitive(g)(name)))(LibMaybes.maybe(LibLogic.ifElse(LibSets.member(name)(tcLambdaVars))(({ tag: "right", value: asVariable }))(LibLogic.ifElse(LibSets.member(name)(inlineVars))(({ tag: "right", value: asVariable }))(LibMaybes.maybe(LibMaybes.maybe(LibMaybes.maybe(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("Unknown variable: ")(((_x) => _x)(name)) }) }))(((_: Core.Term) => ({ tag: "right", value: asFunctionCall })))(LibMaps.lookup(name)(tcMetadata)))(((el: Core.Binding) => (() => {
  const elTrivial1 = Predicates.isTrivialTerm(((_x) => _x.term)(el));
  return LibMaybes.maybe(({ tag: "right", value: asVariable }))(((ts: Core.TypeScheme) => LibLogic.ifElse(LibLogic.and(LibLogic.and(LibEquality.equal(Arity.typeSchemeArity(ts))(0))(Predicates.isComplexBinding(tc)(el)))(LibLogic.not(elTrivial1)))(({ tag: "right", value: asFunctionCall }))((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts))))(makeSimpleLambda(Arity.typeArity(((_x) => _x.type)(ts)))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})())))(((_x) => _x.type)(el));
})()))(Lexical.lookupBinding(g)(name)))(((prim: Graph.Primitive) => (() => {
  const primArity = Arity.primitiveArity(prim);
  return LibLogic.ifElse(LibEquality.equal(primArity)(0))(({ tag: "right", value: asFunctionCall }))((() => {
  const ts = ((_x) => _x.type)(prim);
  return (() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts))))(makeSimpleLambda(Arity.typeArity(((_x) => _x.type)(ts)))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})();
})());
})()))(Lexical.lookupPrimitive(g)(name)))))(((typ: Core.Type) => LibLogic.ifElse(LibSets.member(name)(tcLambdaVars))(({ tag: "right", value: asVariable }))(LibLogic.ifElse(LibSets.member(name)(inlineVars))((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibSets.null_(Variables.freeVariablesInType(typ))))(makeSimpleLambda(Arity.typeArity(typ))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})())(LibLogic.ifElse(LibLogic.not(LibMaps.member(name)(tcMetadata)))(LibMaybes.maybe((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibSets.null_(Variables.freeVariablesInType(typ))))(makeSimpleLambda(Arity.typeArity(typ))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})())(((el: Core.Binding) => (() => {
  const elTrivial = Predicates.isTrivialTerm(((_x) => _x.term)(el));
  return LibMaybes.maybe(LibLogic.ifElse(LibLogic.and(LibEquality.equal(Arity.typeArity(typ))(0))(LibLogic.not(elTrivial)))(({ tag: "right", value: asFunctionCall }))((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibSets.null_(Variables.freeVariablesInType(typ))))(makeSimpleLambda(Arity.typeArity(typ))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})()))(((ts: Core.TypeScheme) => LibLogic.ifElse(LibLogic.and(LibLogic.and(LibEquality.equal(Arity.typeArity(typ))(0))(Predicates.isComplexBinding(tc)(el)))(LibLogic.not(elTrivial)))(({ tag: "right", value: asFunctionCall }))((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibSets.null_(Variables.freeVariablesInType(typ))))(makeSimpleLambda(Arity.typeArity(typ))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})())))(((_x) => _x.type)(el));
})()))(Lexical.lookupBinding(g)(name)))(LibLogic.ifElse(LibLogic.and(LibEquality.equal(Arity.typeArity(typ))(0))(Predicates.isComplexVariable(tc)(name)))(({ tag: "right", value: asFunctionCall }))((() => {
  const asFunctionRef = LibLogic.ifElse(LibLogic.not(LibSets.null_(Variables.freeVariablesInType(typ))))(makeSimpleLambda(Arity.typeArity(typ))(asVariable))(asVariable);
  return ({ tag: "right", value: asFunctionRef });
})()))))))(mTyp));
})();
})();
})();
})();
})();
})();
})();
})();
})();
})())));
}

export function encodeWrappedType<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: Core.Type) => ((x: string | null) => t0 | ReadonlyArray<PythonSyntax.Statement>))) {
  return ((name: Core.Name) => ((typ: Core.Type) => ((comment: string | null) => (() => {
  const tparamList = LibPairs.first(((_x) => _x.boundTypeVariables)(env));
  return LibEithers.bind(encodeTypeQuoted(env)(typ))(((ptypeQuoted: PythonSyntax.Expression) => (() => {
  const pyName = PythonNames.encodeName(false)(({ tag: "pascal" }))(env)(name);
  return (() => {
  const body = PythonUtils.indentedBlock(comment)([]);
  return (() => {
  const typeConstStmt = PythonUtils.dottedAssignmentStatement(pyName)(PythonNames.encodeConstantForTypeName(env)(name))(PythonUtils.functionCall(PythonUtils.pyNameToPyPrimary(PythonNames.encodeName(true)(({ tag: "pascal" }))(env)("hydra.core.Name")))([PythonUtils.doubleQuotedString(((_x) => _x)(name))]));
  return ({ tag: "right", value: [PythonUtils.pyClassDefinitionToPyStatement(({
    decorators: null,
    name: pyName,
    typeParams: LibLists.map(((arg_: Core.Name) => PythonUtils.pyNameToPyTypeParameter(PythonNames.encodeTypeVariable(arg_))))(findTypeParams(env)(typ)),
    arguments: variantArgs(ptypeQuoted)(tparamList),
    body: body
  })), typeConstStmt] });
})();
})();
})()));
})())));
}

export function enumVariantPattern(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: Core.Name) => PythonSyntax.ClosedPattern)) {
  return ((typeName: Core.Name) => ((fieldName: Core.Name) => ({ tag: "value", value: [PythonNames.encodeName(true)(({ tag: "pascal" }))(env)(typeName), PythonNames.encodeEnumValue(env)(fieldName)] })));
}

export function environmentTypeParameters(env: PythonEnvironment.PythonEnvironment): ReadonlyArray<PythonSyntax.TypeParameter> {
  return LibLists.map(((arg_: Core.Name) => PythonUtils.pyNameToPyTypeParameter(PythonNames.encodeTypeVariable(arg_))))(LibPairs.first(((_x) => _x.boundTypeVariables)(env)));
}

export function extendEnvWithLambdaParams(env: PythonEnvironment.PythonEnvironment): ((x: Core.Term) => PythonEnvironment.PythonEnvironment) {
  return ((term: Core.Term) => (() => {
  const go = ((e: PythonEnvironment.PythonEnvironment) => ((t: Core.Term) => (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const newTc = Scoping.extendGraphForLambda(pythonEnvironmentGetGraph(e))(lam);
  return (() => {
  const newEnv = pythonEnvironmentSetGraph(newTc)(e);
  return go(newEnv)(((_x) => _x.body)(lam));
})();
})())((_m as any).value);
    default: return e(_m);
  }
})()));
  return go(env)(term);
})());
}

export function extendEnvWithTypeVar(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => PythonEnvironment.PythonEnvironment) {
  return ((var_: Core.Name) => (() => {
  const oldBound = ((_x) => _x.boundTypeVariables)(env);
  return (() => {
  const tparamList = LibPairs.first(oldBound);
  return (() => {
  const tparamMap = LibPairs.second(oldBound);
  return (() => {
  const newList = LibLists.concat2(tparamList)([var_]);
  return (() => {
  const newMap = LibMaps.insert(var_)(PythonNames.encodeTypeVariable(var_))(tparamMap);
  return ({
    namespaces: ((_x) => _x.namespaces)(env),
    boundTypeVariables: [newList, newMap],
    graph: ((_x) => _x.graph)(env),
    nullaryBindings: ((_x) => _x.nullaryBindings)(env),
    version: ((_x) => _x.version)(env),
    skipCasts: ((_x) => _x.skipCasts)(env),
    inlineVariables: ((_x) => _x.inlineVariables)(env)
  });
})();
})();
})();
})();
})());
}

export function extendMetaForTerm(topLevel: boolean): ((x: PythonEnvironment.PythonModuleMetadata) => ((x: Core.Term) => PythonEnvironment.PythonModuleMetadata)) {
  return ((meta0: PythonEnvironment.PythonModuleMetadata) => ((term: Core.Term) => (() => {
  const step = ((meta: PythonEnvironment.PythonModuleMetadata) => ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "either": return ((e: Core.Term | Core.Term) => (() => {
  const metaWithCast = setMetaUsesCast(true)(meta);
  return LibEithers.either(((_: Core.Term) => setMetaUsesLeft(metaWithCast)(true)))(((_: Core.Term) => setMetaUsesRight(metaWithCast)(true)))(e);
})())((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => LibMaybes.maybe(meta)(((dom: Core.Type) => LibLogic.ifElse(topLevel)(extendMetaForType(true)(false)(dom)(meta))(meta)))(((_x) => _x.domain)(lam)))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return LibLists.foldl((() => {
  const forBinding = ((m: PythonEnvironment.PythonModuleMetadata) => ((b: Core.Binding) => LibMaybes.maybe(m)(((ts: Core.TypeScheme) => (() => {
  const term1 = ((_x) => _x.term)(b);
  return LibLogic.ifElse(Analysis.isSimpleAssignment(term1))(m)(extendMetaForType(true)(true)(((_x) => _x.type)(ts))(m));
})()))(((_x) => _x.type)(b))));
  return forBinding;
})())(meta)(bindings);
})())((_m as any).value);
    case "literal": return ((l: Core.Literal) => (() => {
  const _m = l;
  switch (_m.tag) {
    case "float": return ((fv: Core.FloatValue) => (() => {
  const _m = fv;
  switch (_m.tag) {
    case "bigfloat": return ((_: number) => setMetaUsesDecimal(meta)(true))((_m as any).value);
    default: return meta(_m);
  }
})())((_m as any).value);
    default: return meta(_m);
  }
})())((_m as any).value);
    case "map": return ((_: ReadonlyMap<Core.Term, Core.Term>) => setMetaUsesFrozenDict(meta)(true))((_m as any).value);
    case "maybe": return ((m: Core.Term | null) => LibMaybes.maybe(setMetaUsesNothing(meta)(true))(((_: Core.Term) => setMetaUsesJust(meta)(true)))(m))((_m as any).value);
    case "inject": return ((_: Core.Injection) => setMetaUsesCast(true)(meta))((_m as any).value);
    default: return meta(_m);
  }
})()));
  return Rewriting.foldOverTerm(({ tag: "pre" }))(step)(meta0)(term);
})()));
}

export function extendMetaForType(topLevel: boolean): ((x: boolean) => ((x: Core.Type) => ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PythonModuleMetadata))) {
  return ((isTermAnnot: boolean) => ((typ: Core.Type) => ((meta: PythonEnvironment.PythonModuleMetadata) => (() => {
  const currentTvars = ((_x) => _x.typeVariables)(meta);
  return (() => {
  const newTvars = collectTypeVariables(currentTvars)(typ);
  return (() => {
  const metaWithTvars = setMetaTypeVariables(meta)(newTvars);
  return (() => {
  const metaWithSubtypes = LibLists.foldl(((m: PythonEnvironment.PythonModuleMetadata) => ((t: Core.Type) => extendMetaForType(false)(isTermAnnot)(t)(m))))(metaWithTvars)(Rewriting.subtypes(typ));
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const cod = ((_x) => _x.codomain)(ft);
  return (() => {
  const dom = ((_x) => _x.domain)(ft);
  return (() => {
  const meta2 = extendMetaForType(topLevel)(isTermAnnot)(cod)(metaWithSubtypes);
  return (() => {
  const meta3 = extendMetaForType(false)(isTermAnnot)(dom)(meta2);
  return LibLogic.ifElse(LibLogic.and(isTermAnnot)(topLevel))(meta3)(setMetaUsesCallable(meta3)(true));
})();
})();
})();
})())((_m as any).value);
    case "list": return ((_: Core.Type) => setMetaUsesFrozenList(metaWithSubtypes)(true))((_m as any).value);
    case "map": return ((_: Core.MapType) => setMetaUsesFrozenDict(metaWithSubtypes)(true))((_m as any).value);
    case "maybe": return ((_: Core.Type) => setMetaUsesMaybe(metaWithSubtypes)(true))((_m as any).value);
    case "either": return ((_: Core.EitherType) => setMetaUsesEither(metaWithSubtypes)(true))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => setMetaUsesDecimal(metaWithSubtypes)(true))((_m as any).value);
    default: return metaWithSubtypes(_m);
  }
})())((_m as any).value);
    default: return metaWithSubtypes(_m);
  }
})())((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(Predicates.isEnumRowType(rt))(setMetaUsesEnum(metaWithSubtypes)(true))(LibLogic.ifElse(LibLogic.not(LibLists.null_(rt)))(setMetaUsesNode(metaWithSubtypes)(true))(metaWithSubtypes)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => (() => {
  const body = ((_x) => _x.body)(ft);
  return (() => {
  const metaForWrap = digForWrap(isTermAnnot)(metaWithSubtypes)(body);
  return (() => {
  const _m = Strip.deannotateType(body);
  switch (_m.tag) {
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => setMetaUsesGeneric(metaForWrap)(true))((_m as any).value);
    default: return metaForWrap(_m);
  }
})();
})();
})())((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => (() => {
  const hasAnnotated = LibLists.foldl(((b: boolean) => ((ft: Core.FieldType) => LibLogic.or(b)(Annotations.hasTypeDescription(((_x) => _x.type)(ft))))))(false)(rt);
  return (() => {
  const meta1 = LibLogic.ifElse(LibLists.null_(rt))(metaWithSubtypes)(setMetaUsesDataclass(metaWithSubtypes)(true));
  return LibLogic.ifElse(hasAnnotated)(setMetaUsesAnnotated(meta1)(true))(meta1);
})();
})())((_m as any).value);
    case "wrap": return ((_: Core.Type) => LibLogic.ifElse(isTermAnnot)(metaWithSubtypes)(setMetaUsesNode(metaWithSubtypes)(true)))((_m as any).value);
    default: return metaWithSubtypes(_m);
  }
})();
})();
})();
})();
})())));
}

export function extendMetaForTypes(types: ReadonlyArray<Core.Type>): ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PythonModuleMetadata) {
  return ((meta: PythonEnvironment.PythonModuleMetadata) => (() => {
  const names = LibSets.unions(LibLists.map(((t: Core.Type) => Dependencies.typeDependencyNames(false)(t)))(types));
  return (() => {
  const currentNs = ((_x) => _x.namespaces)(meta);
  return (() => {
  const updatedNs = Analysis.addNamesToNamespaces(PythonNames.encodeNamespace)(names)(currentNs);
  return (() => {
  const meta1 = setMetaNamespaces(updatedNs)(meta);
  return LibLists.foldl(((m: PythonEnvironment.PythonModuleMetadata) => ((t: Core.Type) => extendMetaForType(true)(false)(t)(m))))(meta1)(types);
})();
})();
})();
})());
}

export function extractCaseElimination(term: Core.Term): Core.CaseStatement | null {
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(term);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => cs)((_m as any).value);
    default: return null(_m);
  }
})();
}

export function findTypeParams(env: PythonEnvironment.PythonEnvironment): ((x: Core.Type) => ReadonlyArray<Core.Name>) {
  return ((typ: Core.Type) => (() => {
  const boundVars = LibPairs.second(((_x) => _x.boundTypeVariables)(env));
  return (() => {
  const isBound = ((v: Core.Name) => LibMaybes.isJust(LibMaps.lookup(v)(boundVars)));
  return LibLists.filter(isBound)(LibSets.toList(Variables.freeVariablesInType(typ)));
})();
})());
}

export function gatherLambdas(term: Core.Term): readonly [ReadonlyArray<Core.Name>, Core.Term] {
  return (() => {
  const go = ((params: ReadonlyArray<Core.Name>) => ((t: Core.Term) => (() => {
  const _m = Strip.deannotateAndDetypeTerm(t);
  switch (_m.tag) {
    case "lambda": return ((l: Core.Lambda) => go(LibLists.concat2(params)([((_x) => _x.parameter)(l)]))(((_x) => _x.body)(l)))((_m as any).value);
    default: return [params, t](_m);
  }
})()));
  return go([])(term);
})();
}

export function gatherMetadata(focusNs: Packaging.Namespace): ((x: ReadonlyArray<Packaging.Definition>) => PythonEnvironment.PythonModuleMetadata) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => (() => {
  const start = emptyMetadata(PythonUtils.findNamespaces(focusNs)(defs));
  return (() => {
  const addDef = ((meta: PythonEnvironment.PythonModuleMetadata) => ((def: Packaging.Definition) => (() => {
  const _m = def;
  switch (_m.tag) {
    case "term": return ((termDef: Packaging.TermDefinition) => (() => {
  const term = ((_x) => _x.term)(termDef);
  return (() => {
  const typ = LibMaybes.maybe(({ tag: "variable", value: "hydra.core.Unit" }))(((_x) => _x.type))(((_x) => _x.type)(termDef));
  return (() => {
  const meta2 = extendMetaForType(true)(true)(typ)(meta);
  return extendMetaForTerm(true)(meta2)(term);
})();
})();
})())((_m as any).value);
    case "type": return ((typeDef: Packaging.TypeDefinition) => (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(typeDef));
  return (() => {
  const meta2 = setMetaUsesName(meta)(true);
  return Rewriting.foldOverType(({ tag: "pre" }))(((m: PythonEnvironment.PythonModuleMetadata) => ((t: Core.Type) => extendMetaForType(true)(false)(t)(m))))(meta2)(typ);
})();
})())((_m as any).value);
  }
})()));
  return (() => {
  const result = LibLists.foldl(addDef)(start)(defs);
  return (() => {
  const tvars = ((_x) => _x.typeVariables)(result);
  return (() => {
  const result2 = setMetaUsesCast(true)(setMetaUsesLruCache(true)(result));
  return setMetaUsesTypeVar(result2)(LibLogic.not(LibSets.null_(tvars)));
})();
})();
})();
})();
})());
}

export function genericArg(tparamList: ReadonlyArray<Core.Name>): PythonSyntax.Expression | null {
  return LibLogic.ifElse(LibLists.null_(tparamList))(null)(PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithExpressionSlices(({ tag: "simple", value: ({ tag: "name", value: "Generic" }) }))(LibLists.map(((n: Core.Name) => ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: PythonNames.encodeTypeVariable(n) }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })))(tparamList))));
}

export function initialEnvironment(namespaces: Packaging.Namespaces<PythonSyntax.DottedName>): ((x: Graph.Graph) => PythonEnvironment.PythonEnvironment) {
  return ((tcontext: Graph.Graph) => ({
    namespaces: namespaces,
    boundTypeVariables: [[], LibMaps.empty],
    graph: tcontext,
    nullaryBindings: LibSets.empty,
    version: targetPythonVersion,
    skipCasts: true,
    inlineVariables: LibSets.empty
  }));
}

export function initialMetadata(ns: Packaging.Namespace): PythonEnvironment.PythonModuleMetadata {
  return (() => {
  const dottedNs = PythonNames.encodeNamespace(ns);
  return (() => {
  const emptyNs = ({
    focus: [ns, dottedNs],
    mapping: LibMaps.empty
  });
  return ({
    namespaces: emptyNs,
    typeVariables: LibSets.empty,
    usesAnnotated: false,
    usesCallable: false,
    usesCast: false,
    usesLruCache: false,
    usesTypeAlias: false,
    usesDataclass: false,
    usesDecimal: false,
    usesEither: false,
    usesEnum: false,
    usesFrozenDict: false,
    usesFrozenList: false,
    usesGeneric: false,
    usesJust: false,
    usesLeft: false,
    usesMaybe: false,
    usesName: false,
    usesNode: false,
    usesNothing: false,
    usesRight: false,
    usesTypeVar: false
  });
})();
})();
}

export function isCaseStatementApplication(term: Core.Term): readonly [Core.Name, readonly [Core.Term | null, readonly [ReadonlyArray<Core.Field>, Core.Term]]] | null {
  return (() => {
  const gathered = Analysis.gatherApplications(term);
  return (() => {
  const args = LibPairs.first(gathered);
  return (() => {
  const body = LibPairs.second(gathered);
  return LibLogic.ifElse(LibLogic.not(LibEquality.equal(LibLists.length(args))(1)))(null)((() => {
  const arg = LibLists.head(args);
  return (() => {
  const _m = Strip.deannotateAndDetypeTerm(body);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => [((_x) => _x.typeName)(cs), [((_x) => _x.default)(cs), [((_x) => _x.cases)(cs), arg]]])((_m as any).value);
    default: return null(_m);
  }
})();
})());
})();
})();
})();
}

export function isCasesFull<t0, t1>(rowType: ReadonlyArray<t0>): ((x: ReadonlyArray<t1>) => boolean) {
  return ((cases_: ReadonlyArray<t1>) => (() => {
  const numCases = LibLists.length(cases_);
  return (() => {
  const numFields = LibLists.length(rowType);
  return LibLogic.not(LibEquality.lt(numCases)(numFields));
})();
})());
}

export function isTypeModuleCheck(defs: ReadonlyArray<Packaging.Definition>): boolean {
  return LibLogic.not(LibLists.null_(LibLists.filter(((d: Packaging.Definition) => (() => {
  const _m = d;
  switch (_m.tag) {
    case "type": return ((_: Packaging.TypeDefinition) => true)((_m as any).value);
    default: return false(_m);
  }
})()))(defs)));
}

export function isTypeVariableName(name: Core.Name): boolean {
  return LibEquality.equal(1)(LibLists.length(LibStrings.splitOn(".")(((_x) => _x)(name))));
}

export function isVariantUnitType(rowType: ReadonlyArray<Core.FieldType>): ((x: Core.Name) => boolean) {
  return ((fieldName: Core.Name) => (() => {
  const mfield = LibLists.find(((ft: Core.FieldType) => LibEquality.equal(((_x) => _x.name)(ft))(fieldName)))(rowType);
  return LibMaybes.fromMaybe(false)(LibMaybes.map(((ft: Core.FieldType) => Predicates.isUnitType(Strip.deannotateType(((_x) => _x.type)(ft)))))(mfield));
})());
}

export const lruCacheDecorator: PythonSyntax.NamedExpression = ({ tag: "simple", value: PythonUtils.functionCall(({ tag: "simple", value: ({ tag: "name", value: "lru_cache" }) }))([pyInt(1n)]) });

export function makeCurriedLambda(params: ReadonlyArray<PythonSyntax.Name>): ((x: PythonSyntax.Expression) => PythonSyntax.Expression) {
  return ((body: PythonSyntax.Expression) => LibLists.foldl(((acc: PythonSyntax.Expression) => ((p: PythonSyntax.Name) => ({ tag: "lambda", value: ({
    params: ({
    slashNoDefault: null,
    paramNoDefault: [p],
    paramWithDefault: [],
    starEtc: null
  }),
    body: acc
  }) }))))(body)(LibLists.reverse(params)));
}

export function makePyGraph(g: Graph.Graph): ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PyGraph) {
  return ((m: PythonEnvironment.PythonModuleMetadata) => ({
    graph: g,
    metadata: m
  }));
}

export function makeSimpleLambda(arity: number): ((x: PythonSyntax.Expression) => PythonSyntax.Expression) {
  return ((lhs: PythonSyntax.Expression) => (() => {
  const args = LibLists.map(((i: number) => LibStrings.cat2("x")(LibLiterals.showInt32(i))))(LibMath.range(1)(arity));
  return LibLogic.ifElse(LibEquality.equal(arity)(0))(lhs)(({ tag: "lambda", value: ({
    params: ({
    slashNoDefault: null,
    paramNoDefault: LibLists.map(((a: PythonSyntax.Name) => a))(args),
    paramWithDefault: [],
    starEtc: null
  }),
    body: PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(lhs))(LibLists.map(((a: PythonSyntax.Name) => ({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: a }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] })))(args))
  }) }));
})());
}

export function makeThunk(pbody: PythonSyntax.Expression): PythonSyntax.Expression {
  return PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(PythonUtils.functionCall(({ tag: "simple", value: ({ tag: "name", value: "lru_cache" }) }))([pyInt(1n)])))([wrapInNullaryLambda(pbody)]);
}

export function makeUncurriedLambda(params: ReadonlyArray<PythonSyntax.Name>): ((x: PythonSyntax.Expression) => PythonSyntax.Expression) {
  return ((body: PythonSyntax.Expression) => ({ tag: "lambda", value: ({
    params: ({
    slashNoDefault: null,
    paramNoDefault: LibLists.map(((p: PythonSyntax.Name) => p))(params),
    paramWithDefault: [],
    starEtc: null
  }),
    body: body
  }) }));
}

export function moduleDomainImports(namespaces: Packaging.Namespaces<PythonSyntax.DottedName>): ReadonlyArray<PythonSyntax.ImportStatement> {
  return (() => {
  const names = LibLists.sort(LibMaps.elems(((_x) => _x.mapping)(namespaces)));
  return LibLists.map(((ns: PythonSyntax.DottedName) => ({ tag: "name", value: [({
    name: ns,
    as: null
  })] })))(names);
})();
}

export function moduleImports(namespaces: Packaging.Namespaces<PythonSyntax.DottedName>): ((x: PythonEnvironment.PythonModuleMetadata) => ReadonlyArray<PythonSyntax.Statement>) {
  return ((meta: PythonEnvironment.PythonModuleMetadata) => LibLists.map(((imp: PythonSyntax.ImportStatement) => PythonUtils.pySimpleStatementToPyStatement(({ tag: "import", value: imp }))))(LibLists.concat([moduleStandardImports(meta), moduleDomainImports(namespaces)])));
}

export function moduleStandardImports(meta: PythonEnvironment.PythonModuleMetadata): ReadonlyArray<PythonSyntax.ImportStatement> {
  return (() => {
  const pairs = [["__future__", [condImportSymbol("annotations")(PythonNames.useFutureAnnotations)]], ["collections.abc", [condImportSymbol("Callable")(((_x) => _x.usesCallable)(meta))]], ["dataclasses", [condImportSymbol("dataclass")(((_x) => _x.usesDataclass)(meta))]], ["decimal", [condImportSymbol("Decimal")(((_x) => _x.usesDecimal)(meta))]], ["enum", [condImportSymbol("Enum")(((_x) => _x.usesEnum)(meta))]], ["functools", [condImportSymbol("lru_cache")(((_x) => _x.usesLruCache)(meta))]], ["hydra.dsl.python", [condImportSymbol("Either")(((_x) => _x.usesEither)(meta)), condImportSymbol("FrozenDict")(((_x) => _x.usesFrozenDict)(meta)), condImportSymbol("Just")(((_x) => _x.usesJust)(meta)), condImportSymbol("Left")(((_x) => _x.usesLeft)(meta)), condImportSymbol("Maybe")(((_x) => _x.usesMaybe)(meta)), condImportSymbol("Node")(((_x) => _x.usesNode)(meta)), condImportSymbol("Nothing")(((_x) => _x.usesNothing)(meta)), condImportSymbol("Right")(((_x) => _x.usesRight)(meta)), condImportSymbol("frozenlist")(((_x) => _x.usesFrozenList)(meta))]], ["typing", [condImportSymbol("Annotated")(((_x) => _x.usesAnnotated)(meta)), condImportSymbol("Generic")(((_x) => _x.usesGeneric)(meta)), condImportSymbol("TypeAlias")(((_x) => _x.usesTypeAlias)(meta)), condImportSymbol("TypeVar")(((_x) => _x.usesTypeVar)(meta)), condImportSymbol("cast")(((_x) => _x.usesCast)(meta))]]];
  return (() => {
  const simplified = LibMaybes.cat(LibLists.map(((p: readonly [string, ReadonlyArray<string | null>]) => (() => {
  const modName = LibPairs.first(p);
  return (() => {
  const symbols = LibMaybes.cat(LibPairs.second(p));
  return LibLogic.ifElse(LibLists.null_(symbols))(null)([modName, symbols]);
})();
})()))(pairs));
  return LibLists.map(((p: readonly [string, ReadonlyArray<string>]) => standardImportStatement(LibPairs.first(p))(LibPairs.second(p))))(simplified);
})();
})();
}

export function moduleToPython(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<string, string>))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(encodePythonModule(cx)(g)(mod)(defs))(((file: PythonSyntax.Module) => (() => {
  const s = Serialization.printExpr(Serialization.parenthesize(PythonSerde.encodeModule(file)));
  return (() => {
  const path = Names.namespaceToFilePath(({ tag: "lowerSnake" }))("py")(((_x) => _x.namespace)(mod));
  return ({ tag: "right", value: LibMaps.singleton(path)(s) });
})();
})())))));
}

export function pyGraphGraph(pyg: PythonEnvironment.PyGraph): Graph.Graph {
  return ((_x) => _x.graph)(pyg);
}

export function pyGraphMetadata(pyg: PythonEnvironment.PyGraph): PythonEnvironment.PythonModuleMetadata {
  return ((_x) => _x.metadata)(pyg);
}

export function pyInt(n: bigint): PythonSyntax.Expression {
  return PythonUtils.pyAtomToPyExpression(({ tag: "number", value: ({ tag: "integer", value: n }) }));
}

export function pythonBindingMetadata(g: Graph.Graph): ((x: Core.Binding) => Core.Term | null) {
  return ((b: Core.Binding) => LibLogic.ifElse(shouldThunkBinding(g)(b))(LibLogic.ifElse(Predicates.isComplexBinding(g)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))(null));
}

export function pythonEnvironmentGetGraph(env: PythonEnvironment.PythonEnvironment): Graph.Graph {
  return ((_x) => _x.graph)(env);
}

export function pythonEnvironmentSetGraph(tc: Graph.Graph): ((x: PythonEnvironment.PythonEnvironment) => PythonEnvironment.PythonEnvironment) {
  return ((env: PythonEnvironment.PythonEnvironment) => ({
    namespaces: ((_x) => _x.namespaces)(env),
    boundTypeVariables: ((_x) => _x.boundTypeVariables)(env),
    graph: tc,
    nullaryBindings: ((_x) => _x.nullaryBindings)(env),
    version: ((_x) => _x.version)(env),
    skipCasts: ((_x) => _x.skipCasts)(env),
    inlineVariables: ((_x) => _x.inlineVariables)(env)
  }));
}

export function setMetaNamespaces(ns: Packaging.Namespaces<PythonSyntax.DottedName>): ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PythonModuleMetadata) {
  return ((m: PythonEnvironment.PythonModuleMetadata) => ({
    namespaces: ns,
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaTypeVariables(m: PythonEnvironment.PythonModuleMetadata): ((x: ReadonlySet<Core.Name>) => PythonEnvironment.PythonModuleMetadata) {
  return ((tvars: ReadonlySet<Core.Name>) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: tvars,
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesAnnotated(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: b,
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesCallable(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: b,
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesCast(b: boolean): ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PythonModuleMetadata) {
  return ((m: PythonEnvironment.PythonModuleMetadata) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: b,
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesDataclass(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: b,
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesDecimal(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: b,
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesEither(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: b,
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesEnum(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: b,
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesFrozenDict(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: b,
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesFrozenList(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: b,
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesGeneric(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: b,
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesJust(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: b,
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesLeft(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: b,
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesLruCache(b: boolean): ((x: PythonEnvironment.PythonModuleMetadata) => PythonEnvironment.PythonModuleMetadata) {
  return ((m: PythonEnvironment.PythonModuleMetadata) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: b,
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesMaybe(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: b,
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesName(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: b,
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesNode(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: b,
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesNothing(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: b,
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesRight(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: b,
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesTypeAlias(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: b,
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: ((_x) => _x.usesTypeVar)(m)
  }));
}

export function setMetaUsesTypeVar(m: PythonEnvironment.PythonModuleMetadata): ((x: boolean) => PythonEnvironment.PythonModuleMetadata) {
  return ((b: boolean) => ({
    namespaces: ((_x) => _x.namespaces)(m),
    typeVariables: ((_x) => _x.typeVariables)(m),
    usesAnnotated: ((_x) => _x.usesAnnotated)(m),
    usesCallable: ((_x) => _x.usesCallable)(m),
    usesCast: ((_x) => _x.usesCast)(m),
    usesLruCache: ((_x) => _x.usesLruCache)(m),
    usesTypeAlias: ((_x) => _x.usesTypeAlias)(m),
    usesDataclass: ((_x) => _x.usesDataclass)(m),
    usesDecimal: ((_x) => _x.usesDecimal)(m),
    usesEither: ((_x) => _x.usesEither)(m),
    usesEnum: ((_x) => _x.usesEnum)(m),
    usesFrozenDict: ((_x) => _x.usesFrozenDict)(m),
    usesFrozenList: ((_x) => _x.usesFrozenList)(m),
    usesGeneric: ((_x) => _x.usesGeneric)(m),
    usesJust: ((_x) => _x.usesJust)(m),
    usesLeft: ((_x) => _x.usesLeft)(m),
    usesMaybe: ((_x) => _x.usesMaybe)(m),
    usesName: ((_x) => _x.usesName)(m),
    usesNode: ((_x) => _x.usesNode)(m),
    usesNothing: ((_x) => _x.usesNothing)(m),
    usesRight: ((_x) => _x.usesRight)(m),
    usesTypeVar: b
  }));
}

export function shouldThunkBinding(g: Graph.Graph): ((x: Core.Binding) => boolean) {
  return ((b: Core.Binding) => LibLogic.and(Predicates.isComplexBinding(g)(b))(LibLogic.not(Predicates.isTrivialTerm(((_x) => _x.term)(b)))));
}

export function standardImportStatement(modName: string): ((x: ReadonlyArray<string>) => PythonSyntax.ImportStatement) {
  return ((symbols: ReadonlyArray<string>) => ({ tag: "from", value: ({
    prefixes: [],
    dottedName: [modName],
    targets: ({ tag: "simple", value: LibLists.map(((s: string) => ({
    name: s,
    as: null
  })))(symbols) })
  }) }));
}

export const targetPythonVersion: PythonEnvironment.PythonVersion = PythonUtils.targetPythonVersion;

export function termArityWithPrimitives(graph: Graph.Graph): ((x: Core.Term) => number) {
  return ((term: Core.Term) => (() => {
  const _m = Strip.deannotateAndDetypeTerm(term);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => LibMath.max(0)(LibMath.sub(termArityWithPrimitives(graph)(((_x) => _x.function)(app)))(1)))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => LibMath.add(1)(termArityWithPrimitives(graph)(((_x) => _x.body)(lam))))((_m as any).value);
    case "project": return ((_: Core.Projection) => 1)((_m as any).value);
    case "unwrap": return ((_: Core.Name) => 1)((_m as any).value);
    case "cases": return ((_: Core.CaseStatement) => 1)((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.maybe(0)(((el: Core.Binding) => LibMaybes.maybe(Arity.termArity(((_x) => _x.term)(el)))(((ts: Core.TypeScheme) => Arity.typeSchemeArity(ts)))(((_x) => _x.type)(el))))(Lexical.lookupBinding(graph)(name)))((_m as any).value);
    default: return 0(_m);
  }
})());
}

export function tvarStatement(name: PythonSyntax.Name): PythonSyntax.Statement {
  return PythonUtils.assignmentStatement(name)(PythonUtils.functionCall(({ tag: "simple", value: ({ tag: "name", value: "TypeVar" }) }))([PythonUtils.doubleQuotedString(((_x) => _x)(name))]));
}

export function typeAliasStatementFor(env: PythonEnvironment.PythonEnvironment): ((x: PythonSyntax.Name) => ((x: ReadonlyArray<PythonSyntax.TypeParameter>) => ((x: string | null) => ((x: PythonSyntax.Expression) => PythonSyntax.Statement)))) {
  return ((name: PythonSyntax.Name) => ((tparams: ReadonlyArray<PythonSyntax.TypeParameter>) => ((mcomment: string | null) => ((tyexpr: PythonSyntax.Expression) => LibLogic.ifElse(useInlineTypeParamsFor(((_x) => _x.version)(env)))(PythonUtils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr))(PythonUtils.typeAliasStatement310(name)(tparams)(mcomment)(tyexpr))))));
}

export function unionTypeStatementsFor(env: PythonEnvironment.PythonEnvironment): ((x: PythonSyntax.Name) => ((x: ReadonlyArray<PythonSyntax.TypeParameter>) => ((x: string | null) => ((x: PythonSyntax.Expression) => ((x: ReadonlyArray<PythonSyntax.Statement>) => ReadonlyArray<PythonSyntax.Statement>))))) {
  return ((name: PythonSyntax.Name) => ((tparams: ReadonlyArray<PythonSyntax.TypeParameter>) => ((mcomment: string | null) => ((tyexpr: PythonSyntax.Expression) => ((extraStmts: ReadonlyArray<PythonSyntax.Statement>) => LibLogic.ifElse(useInlineTypeParamsFor(((_x) => _x.version)(env)))(LibLists.concat2([PythonUtils.typeAliasStatement(name)(tparams)(mcomment)(tyexpr)])(extraStmts))(PythonUtils.unionTypeClassStatements310(name)(mcomment)(tyexpr)(extraStmts)))))));
}

export function unsupportedExpression(msg: string): PythonSyntax.Expression {
  return PythonUtils.functionCall(PythonUtils.pyExpressionToPyPrimary(PythonUtils.projectFromExpression(PythonUtils.projectFromExpression(PythonUtils.projectFromExpression(({ tag: "simple", value: [[({ tag: "simple", value: ({
    lhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({
    lhs: null,
    rhs: ({ tag: "simple", value: ({
    lhs: ({
    await: false,
    primary: ({ tag: "simple", value: ({ tag: "name", value: "hydra" }) })
  }),
    rhs: null
  }) })
  })
  })
  })
  })
  })
  }),
    rhs: []
  }) })]] }))("dsl"))("python"))("unsupported")))([PythonUtils.stringToPyExpression(({ tag: "double" }))(msg)]);
}

export const useInlineTypeParams: boolean = useInlineTypeParamsFor(PythonUtils.targetPythonVersion);

export function useInlineTypeParamsFor(version: PythonEnvironment.PythonVersion): boolean {
  return LibEquality.equal(version)(({ tag: "python312" }));
}

export function variantArgs(ptype: PythonSyntax.Expression): ((x: ReadonlyArray<Core.Name>) => PythonSyntax.Args) {
  return ((tparams: ReadonlyArray<Core.Name>) => PythonUtils.pyExpressionsToPyArgs(LibMaybes.cat([PythonUtils.pyPrimaryToPyExpression(PythonUtils.primaryWithExpressionSlices(({ tag: "simple", value: ({ tag: "name", value: "Node" }) }))([ptype])), genericArg(tparams)])));
}

export function variantClosedPattern<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.Name) => ((x: Core.Name) => ((x: PythonSyntax.Name) => ((x: t0) => ((x: boolean) => ((x: Core.Name) => ((x: boolean) => PythonSyntax.ClosedPattern))))))) {
  return ((typeName: Core.Name) => ((fieldName: Core.Name) => ((pyVariantName: PythonSyntax.Name) => ((rowType: t0) => ((isEnum: boolean) => ((varName: Core.Name) => ((shouldCapture: boolean) => LibLogic.ifElse(isEnum)(enumVariantPattern(env)(typeName)(fieldName))(LibLogic.ifElse(LibLogic.not(shouldCapture))(classVariantPatternUnit(pyVariantName))(classVariantPatternWithCapture(env)(pyVariantName)(varName))))))))));
}

export function wildcardCaseBlock(stmt: PythonSyntax.Statement): PythonSyntax.CaseBlock {
  return ({
    patterns: PythonUtils.pyClosedPatternToPyPatterns(({ tag: "wildcard" })),
    guard: null,
    body: PythonUtils.indentedBlock(null)([[stmt]])
  });
}

export function withDefinitions<t0>(env: PythonEnvironment.PythonEnvironment): ((x: ReadonlyArray<Packaging.Definition>) => ((x: ((x: PythonEnvironment.PythonEnvironment) => t0)) => t0)) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((body: ((x: PythonEnvironment.PythonEnvironment) => t0)) => (() => {
  const bindings = LibMaybes.cat(LibLists.map(((def_: Packaging.Definition) => (() => {
  const _m = def_;
  switch (_m.tag) {
    case "term": return ((td: Packaging.TermDefinition) => ({
    name: ((_x) => _x.name)(td),
    term: ((_x) => _x.term)(td),
    type: ((_x) => _x.type)(td)
  }))((_m as any).value);
    case "type": return ((_: Packaging.TypeDefinition) => null)((_m as any).value);
    default: return null(_m);
  }
})()))(defs));
  return (() => {
  const dummyLet = ({
    bindings: bindings,
    body: ({ tag: "literal", value: ({ tag: "string", value: "dummy" }) })
  });
  return withLet(env)(dummyLet)(body);
})();
})()));
}

export function withLambda<t0>(v1: PythonEnvironment.PythonEnvironment): ((x: Core.Lambda) => ((x: ((x: PythonEnvironment.PythonEnvironment) => t0)) => t0)) {
  return ((v2: Core.Lambda) => ((v3: ((x: PythonEnvironment.PythonEnvironment) => t0)) => Environment.withLambdaContext(pythonEnvironmentGetGraph)(pythonEnvironmentSetGraph)(v1)(v2)(v3)));
}

export function withLet<t0>(v1: PythonEnvironment.PythonEnvironment): ((x: Core.Let) => ((x: ((x: PythonEnvironment.PythonEnvironment) => t0)) => t0)) {
  return ((v2: Core.Let) => ((v3: ((x: PythonEnvironment.PythonEnvironment) => t0)) => Environment.withLetContext(pythonEnvironmentGetGraph)(pythonEnvironmentSetGraph)(pythonBindingMetadata)(v1)(v2)(v3)));
}

export function withLetInline<t0>(env: PythonEnvironment.PythonEnvironment): ((x: Core.Let) => ((x: ((x: PythonEnvironment.PythonEnvironment) => t0)) => t0)) {
  return ((lt: Core.Let) => ((body: ((x: PythonEnvironment.PythonEnvironment) => t0)) => (() => {
  const bindingNames = LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(((_x) => _x.bindings)(lt));
  return (() => {
  const inlineVars = LibSets.fromList(bindingNames);
  return (() => {
  const noMetadata = ((tc: t1) => ((b: t2) => null));
  return Environment.withLetContext(pythonEnvironmentGetGraph)(pythonEnvironmentSetGraph)(noMetadata)(env)(lt)(((innerEnv: PythonEnvironment.PythonEnvironment) => (() => {
  const updatedEnv = ({
    namespaces: ((_x) => _x.namespaces)(innerEnv),
    boundTypeVariables: ((_x) => _x.boundTypeVariables)(innerEnv),
    graph: ((_x) => _x.graph)(innerEnv),
    nullaryBindings: ((_x) => _x.nullaryBindings)(innerEnv),
    version: ((_x) => _x.version)(innerEnv),
    skipCasts: ((_x) => _x.skipCasts)(innerEnv),
    inlineVariables: LibSets.union(inlineVars)(((_x) => _x.inlineVariables)(innerEnv))
  });
  return body(updatedEnv);
})()));
})();
})();
})()));
}

export function withTypeLambda<t0>(v1: PythonEnvironment.PythonEnvironment): ((x: Core.TypeLambda) => ((x: ((x: PythonEnvironment.PythonEnvironment) => t0)) => t0)) {
  return ((v2: Core.TypeLambda) => ((v3: ((x: PythonEnvironment.PythonEnvironment) => t0)) => Environment.withTypeLambdaContext(pythonEnvironmentGetGraph)(pythonEnvironmentSetGraph)(v1)(v2)(v3)));
}

export function wrapInNullaryLambda(expr: PythonSyntax.Expression): PythonSyntax.Expression {
  return ({ tag: "lambda", value: ({
    params: ({
    slashNoDefault: null,
    paramNoDefault: [],
    paramWithDefault: [],
    starEtc: null
  }),
    body: expr
  }) });
}

export function wrapLazyArguments(name: Core.Name): ((x: ReadonlyArray<PythonSyntax.Expression>) => ReadonlyArray<PythonSyntax.Expression>) {
  return ((args: ReadonlyArray<PythonSyntax.Expression>) => LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.logic.ifElse"))(LibEquality.equal(LibLists.length(args))(3)))([LibLists.at(0)(args), wrapInNullaryLambda(LibLists.at(1)(args)), wrapInNullaryLambda(LibLists.at(2)(args))])(LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.maybes.cases"))(LibEquality.equal(LibLists.length(args))(3)))([LibLists.at(0)(args), wrapInNullaryLambda(LibLists.at(1)(args)), LibLists.at(2)(args)])(LibLogic.ifElse(LibLogic.and(LibLogic.or(LibEquality.equal(name)("hydra.lib.maybes.maybe"))(LibEquality.equal(name)("hydra.lib.maybes.fromMaybe")))(LibEquality.gte(LibLists.length(args))(1)))(LibLists.cons(wrapInNullaryLambda(LibLists.at(0)(args)))(LibLists.tail(args)))(args))));
}
