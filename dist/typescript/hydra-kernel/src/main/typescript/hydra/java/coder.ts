// Note: this is an automatically generated file. Do not edit.

/**
 * Java code generator: converts Hydra modules to Java source code
 */



import * as Analysis from "../analysis.js";
import * as Annotations from "../annotations.js";
import * as Arity from "../arity.js";
import * as Ast from "../ast.js";
import * as Checking from "../checking.js";
import * as Classes from "../classes.js";
import * as Coders from "../coders.js";
import * as Constants from "../constants.js";
import * as Context from "../context.js";
import * as Core from "../core.js";
import * as DecodeCore from "../decode/core.js";
import * as Dependencies from "../dependencies.js";
import * as EncodeCore from "../encode/core.js";
import * as Environment from "../environment.js";
import * as ErrorChecking from "../error/checking.js";
import * as ErrorCore from "../error/core.js";
import * as ErrorPackaging from "../error/packaging.js";
import * as Errors from "../errors.js";
import * as Formatting from "../formatting.js";
import * as Graph from "../graph.js";
import * as Inference from "../inference.js";
import * as JavaEnvironment from "./environment.js";
import * as JavaLanguage from "./language.js";
import * as JavaNames from "./names.js";
import * as JavaSerde from "./serde.js";
import * as JavaSyntax from "./syntax.js";
import * as JavaUtils from "./utils.js";
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

export function addComment<t0>(decl: JavaSyntax.ClassBodyDeclaration): ((x: Core.FieldType) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments))) {
  return ((field: Core.FieldType) => ((cx: t0) => ((g: Graph.Graph) => LibEithers.map(((c: string | null) => ({
    value: decl,
    comments: c
  })))(Annotations.commentsFromFieldType(cx)(g)(field)))));
}

export function analyzeJavaFunction<t0, t1>(env: JavaEnvironment.JavaEnvironment): ((x: Core.Term) => ((x: Context.Context) => ((x: t0) => t1 | Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>))) {
  return ((term: Core.Term) => ((cx: Context.Context) => ((g: t0) => Analysis.analyzeFunctionTerm(cx)(javaEnvGetGraph)(javaEnvSetGraph)(env)(term))));
}

export function annotateBodyWithCod(typ: Core.Type): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const setAnn = ((t: Core.Term) => Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(typ))(t));
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "typeApplication": return ((_ta: Core.TypeApplicationTerm) => setAnn(term))((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const annotatedRhs = (() => {
  const _m = Strip.deannotateTerm(rhs);
  switch (_m.tag) {
    case "typeApplication": return ((_ta2: Core.TypeApplicationTerm) => annotateBodyWithCod(extractArgType(lhs)(typ))(rhs))((_m as any).value);
    default: return rhs(_m);
  }
})();
  return setAnn(({ tag: "application", value: ({
    function: lhs,
    argument: annotatedRhs
  }) }));
})();
})();
})())((_m as any).value);
    default: return setAnn(term)(_m);
  }
})();
})());
}

export function annotateLambdaArgs<t0, t1>(cname: Core.Name): ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<Core.Term>) => ((x: t0) => ((x: Graph.Graph) => t1 | ReadonlyArray<Core.Term>)))) {
  return ((tApps: ReadonlyArray<Core.Type>) => ((argTerms: ReadonlyArray<Core.Term>) => ((cx: t0) => ((g: Graph.Graph) => LibLogic.ifElse(LibLists.null_(tApps))(({ tag: "right", value: argTerms }))(LibEithers.bind(LibEithers.bind(({ tag: "right", value: Lexical.lookupBinding(g)(cname) }))(((mel: Core.Binding | null) => LibMaybes.cases(mel)(({ tag: "right", value: LibMaybes.map(((prim: Graph.Primitive) => ((_x) => _x.type)(prim)))(LibMaps.lookup(cname)(((_x) => _x.primitives)(g))) }))(((el: Core.Binding) => ({ tag: "right", value: ((_x) => _x.type)(el) }))))))(((mts: Core.TypeScheme | null) => LibMaybes.cases(mts)(({ tag: "right", value: argTerms }))(((ts: Core.TypeScheme) => (() => {
  const schemeType = ((_x) => _x.type)(ts);
  return (() => {
  const schemeTypeVars = collectTypeVars(schemeType);
  return (() => {
  const schemeVars = LibLists.filter(((v: Core.Name) => LibSets.member(v)(schemeTypeVars)))(((_x) => _x.variables)(ts));
  return LibLogic.ifElse(LibLogic.or(LibLists.null_(schemeVars))(LibLogic.not(LibEquality.equal(LibLists.length(schemeVars))(LibLists.length(tApps)))))(({ tag: "right", value: argTerms }))((() => {
  const subst = LibMaps.fromList(LibLists.zip(schemeVars)(tApps));
  return (() => {
  const expectedTypes = peelExpectedTypes(subst)(LibLists.length(argTerms))(schemeType);
  return ({ tag: "right", value: LibLists.zipWith(((arg: Core.Term) => ((mExpected: Core.Type) => propagateType(mExpected)(arg))))(argTerms)(LibLists.concat2(expectedTypes)(LibLists.replicate(LibLists.length(argTerms))(({ tag: "variable", value: "unused" })))) });
})();
})());
})();
})();
})())))))))));
}

export function applyCastIfSafe<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Type) => ((x: JavaSyntax.Expression) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))) {
  return ((castType: Core.Type) => ((expr: JavaSyntax.Expression) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const trusted = ((_x) => _x.trustedTypeVars)(aliases);
  return (() => {
  const inScope = ((_x) => _x.inScopeTypeParams)(aliases);
  return (() => {
  const castVars = collectTypeVars(castType);
  return (() => {
  const javaTypeVars = LibSets.fromList(LibLists.filter(((v: Core.Name) => LibLogic.or(LibSets.member(v)(inScope))(isLambdaBoundVariable(v))))(LibSets.toList(castVars)));
  return (() => {
  const isSafe = LibLogic.or(LibSets.null_(trusted))(LibLogic.or(LibSets.null_(javaTypeVars))(LibSets.null_(LibSets.difference(javaTypeVars)(trusted))));
  return LibLogic.ifElse(isSafe)(LibEithers.bind(encodeType(aliases)(LibSets.empty)(castType)(cx)(g))(((jtype: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(rt)(JavaUtils.javaExpressionToJavaUnaryExpression(expr))) }))))))(({ tag: "right", value: expr }));
})();
})();
})();
})();
})()))));
}

export function applyJavaArg(expr: JavaSyntax.Expression): ((x: JavaSyntax.Expression) => JavaSyntax.Expression) {
  return ((jarg: JavaSyntax.Expression) => JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(({ tag: "right", value: JavaUtils.javaExpressionToJavaPrimary(expr) }))(JavaNames.applyMethodName)([jarg])));
}

export function applyOvergenSubstToTermAnnotations<t0, t1>(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Term) => ((x: t0) => ((x: Graph.Graph) => t1 | Core.Term))) {
  return ((term0: Core.Term) => ((cx: t0) => ((g: Graph.Graph) => ({ tag: "right", value: applyOvergenSubstToTermAnnotations_go(subst)(g)(term0) }))));
}

export function applyOvergenSubstToTermAnnotations_go(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: Graph.Graph) => ((x: Core.Term) => Core.Term)) {
  return ((cx: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const inner = ((_x) => _x.body)(at);
  return (() => {
  const ann = ((_x) => _x.annotation)(at);
  return (() => {
  const ann_ = LibMaybes.cases(LibMaps.lookup(Constants.key_type)(ann))(ann)(((typeTerm: Core.Term) => LibEithers.either(((_: Errors.DecodingError) => ann))(((t: Core.Type) => (() => {
  const t_ = substituteTypeVarsWithTypes(subst)(t);
  return LibMaps.insert(Constants.key_type)(EncodeCore.type(t_))(ann);
})()))(DecodeCore.type(cx)(typeTerm))));
  return ({ tag: "annotated", value: ({
    body: applyOvergenSubstToTermAnnotations_go(subst)(cx)(inner),
    annotation: ann_
  }) });
})();
})();
})())((_m as any).value);
    case "application": return ((app: Core.Application) => ({ tag: "application", value: ({
    function: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.function)(app)),
    argument: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.argument)(app))
  }) }))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(lam),
    domain: LibMaybes.map(((d: Core.Type) => substituteTypeVarsWithTypes(subst)(d)))(((_x) => _x.domain)(lam)),
    body: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.body)(lam))
  }) }))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => ({ tag: "cases", value: ({
    typeName: ((_x) => _x.typeName)(cs),
    default: LibMaybes.map(((d: Core.Term) => applyOvergenSubstToTermAnnotations_go(subst)(cx)(d)))(((_x) => _x.default)(cs)),
    cases: LibLists.map(((fld: Core.Field) => ({
    name: ((_x) => _x.name)(fld),
    term: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.term)(fld))
  })))(((_x) => _x.cases)(cs))
  }) }))((_m as any).value);
    case "let": return ((lt: Core.Let) => ({ tag: "let", value: ({
    bindings: LibLists.map(((b: Core.Binding) => ({
    name: ((_x) => _x.name)(b),
    term: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  })))(((_x) => _x.bindings)(lt)),
    body: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.body)(lt))
  }) }))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => ({ tag: "typeApplication", value: ({
    body: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.body)(ta)),
    type: substituteTypeVarsWithTypes(subst)(((_x) => _x.type)(ta))
  }) }))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => ({ tag: "typeLambda", value: ({
    parameter: ((_x) => _x.parameter)(tl),
    body: applyOvergenSubstToTermAnnotations_go(subst)(cx)(((_x) => _x.body)(tl))
  }) }))((_m as any).value);
    default: return term(_m);
  }
})()));
}

export function applySubstFull(s: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibMaps.findWithDefault(t)(v)(s))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => ({ tag: "function", value: ({
    domain: applySubstFull(s)(((_x) => _x.domain)(ft)),
    codomain: applySubstFull(s)(((_x) => _x.codomain)(ft))
  }) }))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => ({ tag: "application", value: ({
    function: applySubstFull(s)(((_x) => _x.function)(at)),
    argument: applySubstFull(s)(((_x) => _x.argument)(at))
  }) }))((_m as any).value);
    case "list": return ((inner: Core.Type) => ({ tag: "list", value: applySubstFull(s)(inner) }))((_m as any).value);
    case "set": return ((inner: Core.Type) => ({ tag: "set", value: applySubstFull(s)(inner) }))((_m as any).value);
    case "maybe": return ((inner: Core.Type) => ({ tag: "maybe", value: applySubstFull(s)(inner) }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: applySubstFull(s)(((_x) => _x.keys)(mt)),
    values: applySubstFull(s)(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: applySubstFull(s)(((_x) => _x.first)(pt)),
    second: applySubstFull(s)(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: applySubstFull(s)(((_x) => _x.left)(et)),
    right: applySubstFull(s)(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(ft),
    body: applySubstFull(LibMaps.delete_(((_x) => _x.parameter)(ft))(s))(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    default: return t(_m);
  }
})());
}

export function applySubstSimple(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibMaps.findWithDefault(t)(v)(subst))((_m as any).value);
    default: return t(_m);
  }
})());
}

export function arraysCompareExpr(otherVar: string): ((x: string) => JavaSyntax.Expression) {
  return ((fname: string) => (() => {
  const header = ({ tag: "complex", value: ({
    variant: ({ tag: "type", value: JavaUtils.javaTypeName("java.util.Arrays") }),
    typeArguments: [],
    identifier: "compare"
  }) });
  const arg1 = JavaUtils.javaExpressionNameToJavaExpression(({
    qualifier: null,
    identifier: JavaUtils.sanitizeJavaName(fname)
  }));
  const arg2 = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.javaIdentifier(otherVar))(JavaUtils.javaIdentifier(fname)));
  return JavaUtils.javaMethodInvocationToJavaExpression(({
    header: header,
    arguments: [arg1, arg2]
  }));
})());
}

export function arraysEqualsClause(tmpName: string): ((x: string) => JavaSyntax.InclusiveOrExpression) {
  return ((fname: string) => (() => {
  const thisArg = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression("this")(JavaUtils.javaIdentifier(fname)));
  const otherArg = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.javaIdentifier(tmpName))(JavaUtils.javaIdentifier(fname)));
  const header = ({ tag: "complex", value: ({
    variant: ({ tag: "type", value: JavaUtils.javaTypeName("java.util.Arrays") }),
    typeArguments: [],
    identifier: JavaNames.equalsMethodName
  }) });
  return JavaUtils.javaPostfixExpressionToJavaInclusiveOrExpression(JavaUtils.javaMethodInvocationToJavaPostfixExpression(({
    header: header,
    arguments: [thisArg, otherArg]
  })));
})());
}

export function augmentVariantClass(aliases: JavaEnvironment.Aliases): ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: JavaSyntax.ClassDeclaration) => JavaSyntax.ClassDeclaration))) {
  return ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((cd: JavaSyntax.ClassDeclaration) => (() => {
  const _m = cd;
  switch (_m.tag) {
    case "normal": return ((ncd: JavaSyntax.NormalClassDeclaration) => (() => {
  const args = LibLists.map(((tp: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToTypeArgument(tp)))(tparams);
  return (() => {
  const extendsPart = JavaUtils.nameToJavaClassType(aliases)(true)(args)(elName)(null);
  return (() => {
  const newMods = [({ tag: "public" }), ({ tag: "static" }), ({ tag: "final" })];
  return (() => {
  const oldBody = ((_x) => _x.body)(ncd);
  return (() => {
  const oldDecls = ((_x) => _x)(oldBody);
  return (() => {
  const acceptDecl = noComment(JavaUtils.toAcceptMethod(false)(tparams));
  return (() => {
  const newBody = LibLists.concat2(oldDecls)([acceptDecl]);
  return ({ tag: "normal", value: ({
    modifiers: newMods,
    identifier: ((_x) => _x.identifier)(ncd),
    parameters: tparams,
    extends: extendsPart,
    implements: ((_x) => _x.implements)(ncd),
    body: newBody
  }) });
})();
})();
})();
})();
})();
})();
})())((_m as any).value);
    default: return cd(_m);
  }
})())));
}

export function bindingIsFunctionType(b: Core.Binding): boolean {
  return LibMaybes.maybe((() => {
  const _m = Strip.deannotateTerm(((_x) => _x.term)(b));
  switch (_m.tag) {
    case "lambda": return ((_f: Core.Lambda) => true)((_m as any).value);
    case "project": return ((_f: Core.Projection) => true)((_m as any).value);
    case "cases": return ((_f: Core.CaseStatement) => true)((_m as any).value);
    case "unwrap": return ((_f: Core.Name) => true)((_m as any).value);
    default: return false(_m);
  }
})())(((ts: Core.TypeScheme) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.type)(ts));
  switch (_m.tag) {
    case "function": return ((_ft: Core.FunctionType) => true)((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.body)(fa));
  switch (_m.tag) {
    case "function": return ((_ft2: Core.FunctionType) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})()))(((_x) => _x.type)(b));
}

export function bindingNameToFilePath(name: Core.Name): string {
  return (() => {
  const qn = Names.qualifyName(name);
  const ns_ = ((_x) => _x.namespace)(qn);
  const local = ((_x) => _x.local)(qn);
  const sanitized = Formatting.sanitizeWithUnderscores(JavaLanguage.reservedWords)(local);
  const unq = Names.unqualifyName(({
    namespace: ns_,
    local: sanitized
  }));
  return Names.nameToFilePath(({ tag: "camel" }))(({ tag: "pascal" }))("java")(unq);
})();
}

export function bindingsToStatements(env: JavaEnvironment.JavaEnvironment): ((x: ReadonlyArray<Core.Binding>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]))) {
  return ((bindings: ReadonlyArray<Core.Binding>) => ((cx: Context.Context) => ((g0: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const g = ((_x) => _x.graph)(env);
  return (() => {
  const flatBindings = dedupBindings(((_x) => _x.inScopeJavaVars)(aliases))(flattenBindings(bindings));
  return (() => {
  const gExtended = Scoping.extendGraphForLet(((g2: Graph.Graph) => ((b: Core.Binding) => LibLogic.ifElse(Predicates.isComplexBinding(g2)(b))(({ tag: "literal", value: ({ tag: "boolean", value: true }) }))(null))))(g)(({
    bindings: flatBindings,
    body: ({ tag: "variable", value: "dummy" })
  }));
  return (() => {
  const bindingVars = LibSets.fromList(LibLists.map(((b: Core.Binding) => ((_x) => _x.name)(b)))(flatBindings));
  return (() => {
  const allDeps = LibMaps.fromList(LibLists.map(((b: Core.Binding) => (() => {
  const key = ((_x) => _x.name)(b);
  return (() => {
  const deps = LibSets.intersection(bindingVars)(Variables.freeVariablesInTerm(((_x) => _x.term)(b)));
  return [key, deps];
})();
})()))(flatBindings));
  return (() => {
  const sorted = Sorting.topologicalSortComponents(LibLists.map(((entry: readonly [Core.Name, ReadonlySet<Core.Name>]) => (() => {
  const key = LibPairs.first(entry);
  return (() => {
  const deps = LibPairs.second(entry);
  return [key, LibSets.toList(deps)];
})();
})()))(LibMaps.toList(allDeps)));
  return (() => {
  const recursiveVars = LibSets.fromList(LibLists.concat(LibLists.map(((names: ReadonlyArray<Core.Name>) => LibLogic.ifElse(LibEquality.equal(LibLists.length(names))(1))((() => {
  const singleName = LibLists.head(names);
  return LibMaybes.cases(LibMaps.lookup(singleName)(allDeps))([])(((deps: ReadonlySet<Core.Name>) => LibLogic.ifElse(LibSets.member(singleName)(deps))([singleName])([])));
})())(names)))(sorted)));
  return (() => {
  const thunkedVars = LibSets.fromList(LibLists.concat(LibLists.map(((b: Core.Binding) => (() => {
  const bname = ((_x) => _x.name)(b);
  return LibLogic.ifElse(LibLogic.and(LibLogic.not(LibSets.member(bname)(recursiveVars)))(LibLogic.and(needsThunking(((_x) => _x.term)(b)))(LibLogic.not(bindingIsFunctionType(b)))))([bname])([]);
})()))(flatBindings)));
  return (() => {
  const aliasesExtended = ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases),
    packages: ((_x) => _x.packages)(aliases),
    branchVars: ((_x) => _x.branchVars)(aliases),
    recursiveVars: LibSets.union(((_x) => _x.recursiveVars)(aliases))(recursiveVars),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases),
    inScopeJavaVars: LibSets.union(((_x) => _x.inScopeJavaVars)(aliases))(bindingVars),
    varRenames: ((_x) => _x.varRenames)(aliases),
    lambdaVars: ((_x) => _x.lambdaVars)(aliases),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases),
    methodCodomain: ((_x) => _x.methodCodomain)(aliases),
    thunkedVars: LibSets.union(((_x) => _x.thunkedVars)(aliases))(thunkedVars)
  });
  return (() => {
  const envExtended = ({
    aliases: aliasesExtended,
    graph: gExtended
  });
  return LibLogic.ifElse(LibLists.null_(bindings))(({ tag: "right", value: [[], envExtended] }))(LibEithers.bind(LibEithers.mapList(((names: ReadonlyArray<Core.Name>) => LibEithers.bind(LibEithers.mapList(((n: Core.Name) => toDeclInit(aliasesExtended)(gExtended)(recursiveVars)(flatBindings)(n)(cx)(g)))(names))(((inits: ReadonlyArray<JavaSyntax.BlockStatement | null>) => LibEithers.bind(LibEithers.mapList(((n: Core.Name) => toDeclStatement(envExtended)(aliasesExtended)(gExtended)(recursiveVars)(thunkedVars)(flatBindings)(n)(cx)(g)))(names))(((decls: ReadonlyArray<JavaSyntax.BlockStatement>) => ({ tag: "right", value: LibLists.concat2(LibMaybes.cat(inits))(decls) })))))))(sorted))(((groups: ReadonlyArray<ReadonlyArray<JavaSyntax.BlockStatement>>) => ({ tag: "right", value: [LibLists.concat(groups), envExtended] }))));
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
})())));
}

export function boundTypeVariables(typ: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = typ;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedType) => boundTypeVariables(((_x) => _x.body)(at)))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(ft))(boundTypeVariables(((_x) => _x.body)(ft))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function buildArgSubst<t0>(schemeVarSet: ReadonlySet<Core.Name>): ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyArray<t0>) => ReadonlyMap<Core.Name, t0>)) {
  return ((schemeDoms: ReadonlyArray<Core.Type>) => ((argTypes: ReadonlyArray<t0>) => LibMaps.fromList(LibLists.bind(LibLists.zip(schemeDoms)(argTypes))(((p: readonly [Core.Type, t0]) => (() => {
  const sdom = LibPairs.first(p);
  return (() => {
  const argType = LibPairs.second(p);
  return (() => {
  const _m = Strip.deannotateType(sdom);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibSets.member(v)(schemeVarSet))([[v, argType]])([]))((_m as any).value);
    default: return [](_m);
  }
})();
})();
})())))));
}

export function buildCurriedLambda(params: ReadonlyArray<Core.Name>): ((x: JavaSyntax.Expression) => JavaSyntax.Expression) {
  return ((inner: JavaSyntax.Expression) => LibLists.foldl(((acc: JavaSyntax.Expression) => ((p: Core.Name) => JavaUtils.javaLambda(p)(acc))))(inner)(LibLists.reverse(params)));
}

export function buildSubstFromAnnotations<t0, t1>(schemeVarSet: ReadonlySet<Core.Name>): ((x: Core.Term) => ((x: t0) => ((x: Graph.Graph) => t1 | ReadonlyMap<Core.Name, Core.Name>))) {
  return ((term: Core.Term) => ((cx: t0) => ((g: Graph.Graph) => ({ tag: "right", value: buildSubstFromAnnotations_go(schemeVarSet)(g)(term) }))));
}

export function buildSubstFromAnnotations_go(schemeVarSet: ReadonlySet<Core.Name>): ((x: Graph.Graph) => ((x: Core.Term) => ReadonlyMap<Core.Name, Core.Name>)) {
  return ((g: Graph.Graph) => ((term: Core.Term) => (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => (() => {
  const body = ((_x) => _x.body)(at);
  return (() => {
  const anns = ((_x) => _x.annotation)(at);
  return (() => {
  const bodySubst = buildSubstFromAnnotations_go(schemeVarSet)(g)(body);
  return (() => {
  const annSubst = LibMaybes.cases(LibMaps.lookup(Constants.key_type)(anns))(LibMaps.empty)(((typeTerm: Core.Term) => LibEithers.either(((_: Errors.DecodingError) => LibMaps.empty))(((annType: Core.Type) => (() => {
  const _m = Strip.deannotateTerm(body);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibMaybes.cases(((_x) => _x.domain)(lam))(LibMaps.empty)(((dom: Core.Type) => (() => {
  const _m = Strip.deannotateType(annType);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => buildTypeVarSubst(schemeVarSet)(((_x) => _x.domain)(ft))(dom))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})()))(DecodeCore.type(g)(typeTerm))));
  return LibMaps.union(annSubst)(bodySubst);
})();
})();
})();
})())((_m as any).value);
    case "application": return ((app: Core.Application) => LibMaps.union(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.function)(app)))(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.argument)(app))))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.body)(lam)))((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const defSubst = LibMaybes.cases(((_x) => _x.default)(cs))(LibMaps.empty)(((d: Core.Term) => buildSubstFromAnnotations_go(schemeVarSet)(g)(d)));
  return (() => {
  const caseSubsts = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((fld: Core.Field) => LibMaps.union(acc)(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.term)(fld))))))(LibMaps.empty)(((_x) => _x.cases)(cs));
  return LibMaps.union(defSubst)(caseSubsts);
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindingSubst = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((b: Core.Binding) => LibMaps.union(acc)(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.term)(b))))))(LibMaps.empty)(((_x) => _x.bindings)(lt));
  return LibMaps.union(bindingSubst)(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.body)(lt)));
})())((_m as any).value);
    case "list": return ((terms: ReadonlyArray<Core.Term>) => LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((t: Core.Term) => LibMaps.union(acc)(buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))))(LibMaps.empty)(terms))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.cases(mt)(LibMaps.empty)(((t: Core.Term) => buildSubstFromAnnotations_go(schemeVarSet)(g)(t))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibMaps.union(buildSubstFromAnnotations_go(schemeVarSet)(g)(LibPairs.first(p)))(buildSubstFromAnnotations_go(schemeVarSet)(g)(LibPairs.second(p))))((_m as any).value);
    case "record": return ((r: Core.Record) => LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((fld: Core.Field) => LibMaps.union(acc)(buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.term)(fld))))))(LibMaps.empty)(((_x) => _x.fields)(r)))((_m as any).value);
    case "set": return ((terms: ReadonlySet<Core.Term>) => LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Name>) => ((t: Core.Term) => LibMaps.union(acc)(buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))))(LibMaps.empty)(LibSets.toList(terms)))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.body)(ta)))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => buildSubstFromAnnotations_go(schemeVarSet)(g)(((_x) => _x.body)(tl)))((_m as any).value);
    case "either": return ((e: Core.Term | Core.Term) => LibEithers.either(((t: Core.Term) => buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(((t: Core.Term) => buildSubstFromAnnotations_go(schemeVarSet)(g)(t)))(e))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})()));
}

export function buildTypeSubst(schemeVarSet: ReadonlySet<Core.Name>): ((x: Core.Type) => ((x: Core.Type) => ReadonlyMap<Core.Name, Core.Type>)) {
  return ((schemeType: Core.Type) => ((actualType: Core.Type) => buildTypeSubst_go(schemeVarSet)(Strip.deannotateType(schemeType))(Strip.deannotateType(actualType))));
}

export function buildTypeSubst_go(svs: ReadonlySet<Core.Name>): ((x: Core.Type) => ((x: Core.Type) => ReadonlyMap<Core.Name, Core.Type>)) {
  return ((st: Core.Type) => ((at: Core.Type) => (() => {
  const goSub = ((a: Core.Type) => ((b: Core.Type) => buildTypeSubst_go(svs)(Strip.deannotateType(a))(Strip.deannotateType(b))));
  return (() => {
  const _m = st;
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibLogic.ifElse(LibSets.member(v)(svs))(LibMaps.singleton(v)(at))(LibMaps.empty))((_m as any).value);
    case "function": return ((sft: Core.FunctionType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "function": return ((aft: Core.FunctionType) => LibMaps.union(goSub(((_x) => _x.domain)(sft))(((_x) => _x.domain)(aft)))(goSub(((_x) => _x.codomain)(sft))(((_x) => _x.codomain)(aft))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "application": return ((sat: Core.ApplicationType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "application": return ((aat: Core.ApplicationType) => LibMaps.union(goSub(((_x) => _x.function)(sat))(((_x) => _x.function)(aat)))(goSub(((_x) => _x.argument)(sat))(((_x) => _x.argument)(aat))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "list": return ((sl: Core.Type) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "list": return ((al: Core.Type) => goSub(sl)(al))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "set": return ((ss: Core.Type) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "set": return ((as_: Core.Type) => goSub(ss)(as_))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "maybe": return ((sm: Core.Type) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "maybe": return ((am: Core.Type) => goSub(sm)(am))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "map": return ((smt: Core.MapType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "map": return ((amt: Core.MapType) => LibMaps.union(goSub(((_x) => _x.keys)(smt))(((_x) => _x.keys)(amt)))(goSub(((_x) => _x.values)(smt))(((_x) => _x.values)(amt))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "pair": return ((spt: Core.PairType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "pair": return ((apt: Core.PairType) => LibMaps.union(goSub(((_x) => _x.first)(spt))(((_x) => _x.first)(apt)))(goSub(((_x) => _x.second)(spt))(((_x) => _x.second)(apt))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "either": return ((set_: Core.EitherType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "either": return ((aet: Core.EitherType) => LibMaps.union(goSub(((_x) => _x.left)(set_))(((_x) => _x.left)(aet)))(goSub(((_x) => _x.right)(set_))(((_x) => _x.right)(aet))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "forall": return ((sfa: Core.ForallType) => (() => {
  const _m = at;
  switch (_m.tag) {
    case "forall": return ((afa: Core.ForallType) => goSub(((_x) => _x.body)(sfa))(((_x) => _x.body)(afa)))((_m as any).value);
    default: return goSub(((_x) => _x.body)(sfa))(at)(_m);
  }
})())((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})();
})()));
}

export function buildTypeVarSubst(schemeVarSet: ReadonlySet<Core.Name>): ((x: Core.Type) => ((x: Core.Type) => ReadonlyMap<Core.Name, Core.Name>)) {
  return ((freshTyp: Core.Type) => ((canonTyp: Core.Type) => buildTypeVarSubst_go(schemeVarSet)(Strip.deannotateType(freshTyp))(Strip.deannotateType(canonTyp))));
}

export function buildTypeVarSubst_go(svs: ReadonlySet<Core.Name>): ((x: Core.Type) => ((x: Core.Type) => ReadonlyMap<Core.Name, Core.Name>)) {
  return ((ft: Core.Type) => ((ct: Core.Type) => (() => {
  const goSub = ((a: Core.Type) => ((b: Core.Type) => buildTypeVarSubst_go(svs)(Strip.deannotateType(a))(Strip.deannotateType(b))));
  return (() => {
  const _m = ft;
  switch (_m.tag) {
    case "variable": return ((fn: Core.Name) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "variable": return ((cn: Core.Name) => LibLogic.ifElse(LibLogic.and(LibLogic.not(LibEquality.equal(fn)(cn)))(LibSets.member(cn)(svs)))(LibMaps.singleton(fn)(cn))(LibMaps.empty))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "function": return ((fft: Core.FunctionType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "function": return ((cft: Core.FunctionType) => LibMaps.union(goSub(((_x) => _x.domain)(fft))(((_x) => _x.domain)(cft)))(goSub(((_x) => _x.codomain)(fft))(((_x) => _x.codomain)(cft))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "application": return ((fat: Core.ApplicationType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "application": return ((cat: Core.ApplicationType) => LibMaps.union(goSub(((_x) => _x.function)(fat))(((_x) => _x.function)(cat)))(goSub(((_x) => _x.argument)(fat))(((_x) => _x.argument)(cat))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "list": return ((fl: Core.Type) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "list": return ((cl: Core.Type) => goSub(fl)(cl))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "set": return ((fs: Core.Type) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "set": return ((cs: Core.Type) => goSub(fs)(cs))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "maybe": return ((fm: Core.Type) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "maybe": return ((cm: Core.Type) => goSub(fm)(cm))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "map": return ((fmt: Core.MapType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "map": return ((cmt: Core.MapType) => LibMaps.union(goSub(((_x) => _x.keys)(fmt))(((_x) => _x.keys)(cmt)))(goSub(((_x) => _x.values)(fmt))(((_x) => _x.values)(cmt))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "pair": return ((fpt: Core.PairType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "pair": return ((cpt: Core.PairType) => LibMaps.union(goSub(((_x) => _x.first)(fpt))(((_x) => _x.first)(cpt)))(goSub(((_x) => _x.second)(fpt))(((_x) => _x.second)(cpt))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "either": return ((fet: Core.EitherType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "either": return ((cet: Core.EitherType) => LibMaps.union(goSub(((_x) => _x.left)(fet))(((_x) => _x.left)(cet)))(goSub(((_x) => _x.right)(fet))(((_x) => _x.right)(cet))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})())((_m as any).value);
    case "forall": return ((ffa: Core.ForallType) => (() => {
  const _m = ct;
  switch (_m.tag) {
    case "forall": return ((cfa: Core.ForallType) => goSub(((_x) => _x.body)(ffa))(((_x) => _x.body)(cfa)))((_m as any).value);
    default: return buildTypeVarSubst_go(svs)(Strip.deannotateType(((_x) => _x.body)(ffa)))(ct)(_m);
  }
})())((_m as any).value);
    default: return (() => {
  const _m = ct;
  switch (_m.tag) {
    case "forall": return ((cfa: Core.ForallType) => buildTypeVarSubst_go(svs)(ft)(Strip.deannotateType(((_x) => _x.body)(cfa))))((_m as any).value);
    default: return LibMaps.empty(_m);
  }
})()(_m);
  }
})();
})()));
}

export const classModsPublic: ReadonlyArray<JavaSyntax.ClassModifier> = [({ tag: "public" })];

export function classifyDataReference<t0>(name: Core.Name): ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaEnvironment.JavaSymbolClass)) {
  return ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(({ tag: "right", value: Lexical.lookupBinding(g)(name) }))(((mel: Core.Binding | null) => LibMaybes.cases(mel)(({ tag: "right", value: ({ tag: "localVariable" }) }))(((el: Core.Binding) => LibMaybes.cases(((_x) => _x.type)(el))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("no type scheme for element ")(((_x) => _x)(((_x) => _x.name)(el))) }) }))(((ts: Core.TypeScheme) => ({ tag: "right", value: classifyDataTerm(ts)(((_x) => _x.term)(el)) })))))))));
}

export function classifyDataTerm(ts: Core.TypeScheme): ((x: Core.Term) => JavaEnvironment.JavaSymbolClass) {
  return ((term: Core.Term) => LibLogic.ifElse(Dependencies.isLambda(term))((() => {
  const n = classifyDataTerm_countLambdaParams(term);
  return LibLogic.ifElse(LibEquality.gt(n)(1))(({ tag: "hoistedLambda", value: n }))(({ tag: "unaryFunction" }));
})())((() => {
  const hasTypeParams = LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts)));
  return LibLogic.ifElse(hasTypeParams)((() => {
  const n2 = classifyDataTerm_countLambdaParams(classifyDataTerm_stripTypeLambdas(term));
  return LibLogic.ifElse(LibEquality.gt(n2)(0))(({ tag: "hoistedLambda", value: n2 }))(({ tag: "nullaryFunction" }));
})())(({ tag: "nullaryFunction" }));
})()));
}

export function classifyDataTerm_countLambdaParams(t: Core.Term): number {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibMath.add(1)(classifyDataTerm_countLambdaParams(((_x) => _x.body)(lam))))((_m as any).value);
    case "let": return ((lt: Core.Let) => classifyDataTerm_countLambdaParams(((_x) => _x.body)(lt)))((_m as any).value);
    default: return 0(_m);
  }
})();
}

export function classifyDataTerm_stripTypeLambdas(t: Core.Term): Core.Term {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "typeLambda": return ((tl: Core.TypeLambda) => classifyDataTerm_stripTypeLambdas(((_x) => _x.body)(tl)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function cmpDeclStatement<t0>(aliases: t0): JavaSyntax.BlockStatement {
  return JavaUtils.variableDeclarationStatement(aliases)(JavaUtils.javaIntType)(JavaUtils.javaIdentifier("cmp"))(JavaUtils.javaIntExpression(0n));
}

export const cmpNotZeroExpr: JavaSyntax.Expression = (() => {
  const lhs = JavaUtils.javaRelationalExpressionToJavaEqualityExpression(JavaUtils.javaPostfixExpressionToJavaRelationalExpression(({ tag: "name", value: ({
    qualifier: null,
    identifier: JavaUtils.javaIdentifier("cmp")
  }) })));
  const rhs = JavaUtils.javaPostfixExpressionToJavaRelationalExpression(({ tag: "primary", value: JavaUtils.javaLiteralToJavaPrimary(JavaUtils.javaInt(0n)) }));
  return JavaUtils.javaEqualityExpressionToJavaExpression(({ tag: "notEqual", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
})();

export function collectForallParams(t: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((fa: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(fa))(collectForallParams(((_x) => _x.body)(fa))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function collectLambdaDomains(t: Core.Term): readonly [ReadonlyArray<Core.Type>, Core.Term] {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibMaybes.cases(((_x) => _x.domain)(lam))([[], t])(((dom: Core.Type) => (() => {
  const rest = collectLambdaDomains(((_x) => _x.body)(lam));
  return [LibLists.cons(dom)(LibPairs.first(rest)), LibPairs.second(rest)];
})())))((_m as any).value);
    default: return [[], t](_m);
  }
})();
}

export function collectTypeApps(t: Core.Term): ((x: ReadonlyArray<Core.Type>) => readonly [Core.Term, ReadonlyArray<Core.Type>]) {
  return ((acc: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => collectTypeApps(((_x) => _x.body)(ta))(LibLists.cons(((_x) => _x.type)(ta))(acc)))((_m as any).value);
    default: return [Strip.deannotateTerm(t), acc](_m);
  }
})());
}

export function collectTypeApps0(t: Core.Term): ((x: ReadonlyArray<Core.Type>) => readonly [Core.Term, ReadonlyArray<Core.Type>]) {
  return ((acc: ReadonlyArray<Core.Type>) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => collectTypeApps0(((_x) => _x.body)(ta))(LibLists.cons(((_x) => _x.type)(ta))(acc)))((_m as any).value);
    default: return [t, acc](_m);
  }
})());
}

export function collectTypeVars(typ: Core.Type): ReadonlySet<Core.Name> {
  return collectTypeVars_go(Strip.deannotateType(typ));
}

export function collectTypeVars_go(t: Core.Type): ReadonlySet<Core.Name> {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => LibSets.singleton(name))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => LibSets.union(collectTypeVars_go(Strip.deannotateType(((_x) => _x.domain)(ft))))(collectTypeVars_go(Strip.deannotateType(((_x) => _x.codomain)(ft)))))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => LibSets.union(collectTypeVars_go(Strip.deannotateType(((_x) => _x.function)(at))))(collectTypeVars_go(Strip.deannotateType(((_x) => _x.argument)(at)))))((_m as any).value);
    case "list": return ((inner: Core.Type) => collectTypeVars_go(Strip.deannotateType(inner)))((_m as any).value);
    case "set": return ((inner: Core.Type) => collectTypeVars_go(Strip.deannotateType(inner)))((_m as any).value);
    case "maybe": return ((inner: Core.Type) => collectTypeVars_go(Strip.deannotateType(inner)))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibSets.union(collectTypeVars_go(Strip.deannotateType(((_x) => _x.keys)(mt))))(collectTypeVars_go(Strip.deannotateType(((_x) => _x.values)(mt)))))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibSets.union(collectTypeVars_go(Strip.deannotateType(((_x) => _x.first)(pt))))(collectTypeVars_go(Strip.deannotateType(((_x) => _x.second)(pt)))))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibSets.union(collectTypeVars_go(Strip.deannotateType(((_x) => _x.left)(et))))(collectTypeVars_go(Strip.deannotateType(((_x) => _x.right)(et)))))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => collectTypeVars_go(Strip.deannotateType(((_x) => _x.body)(ft))))((_m as any).value);
    default: return LibSets.empty(_m);
  }
})();
}

export function comparableCompareExpr(otherVar: string): ((x: string) => JavaSyntax.Expression) {
  return ((fname: string) => (() => {
  const thisField = JavaUtils.javaIdentifierToJavaExpression(JavaUtils.sanitizeJavaName(fname));
  const otherField = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.javaIdentifier(otherVar))(JavaUtils.javaIdentifier(fname)));
  return JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("hydra.util.Comparing")("compare")([thisField, otherField]));
})());
}

export function compareAndReturnStmts(otherVar: string): ((x: Core.FieldType) => ReadonlyArray<JavaSyntax.BlockStatement>) {
  return ((f: Core.FieldType) => [({ tag: "statement", value: JavaUtils.javaAssignmentStatement(({ tag: "expressionName", value: ({
    qualifier: null,
    identifier: JavaUtils.javaIdentifier("cmp")
  }) }))(compareFieldExpr(otherVar)(f)) }), ({ tag: "statement", value: ({ tag: "ifThen", value: ({
    expression: cmpNotZeroExpr,
    statement: JavaUtils.javaReturnStatement(JavaUtils.javaExpressionNameToJavaExpression(({
    qualifier: null,
    identifier: JavaUtils.javaIdentifier("cmp")
  })))
  }) }) })]);
}

export function compareFieldExpr(otherVar: string): ((x: Core.FieldType) => JavaSyntax.Expression) {
  return ((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(ft));
  return (() => {
  const ftype = ((_x) => _x.type)(ft);
  return LibLogic.ifElse(isBinaryType(ftype))(arraysCompareExpr(otherVar)(fname))(LibLogic.ifElse(isNonComparableType(ftype))(hashCodeCompareExpr(otherVar)(fname))(comparableCompareExpr(otherVar)(fname)));
})();
})());
}

export function compareToBody<t0>(aliases: t0): ((x: string) => ((x: ReadonlyArray<Core.FieldType>) => ReadonlyArray<JavaSyntax.BlockStatement>)) {
  return ((otherVar: string) => ((fields: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(LibLists.null_(fields))([({ tag: "statement", value: JavaUtils.javaReturnStatement(JavaUtils.javaIntExpression(0n)) })])(LibLogic.ifElse(LibEquality.equal(LibLists.length(fields))(1))([({ tag: "statement", value: JavaUtils.javaReturnStatement(compareFieldExpr(otherVar)(LibLists.head(fields))) })])(LibLists.concat2([cmpDeclStatement(aliases)])(LibLists.concat2(LibLists.concat(LibLists.map(((f: Core.FieldType) => compareAndReturnStmts(otherVar)(f)))(LibLists.init(fields))))([({ tag: "statement", value: JavaUtils.javaReturnStatement(compareFieldExpr(otherVar)(LibLists.last(fields))) })]))))));
}

export function compareToZeroClause(tmpName: string): ((x: string) => JavaSyntax.InclusiveOrExpression) {
  return ((fname: string) => (() => {
  const compareToArg = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.javaIdentifier(tmpName))(JavaUtils.javaIdentifier(fname)));
  const compareToVar = ({ tag: "expression", value: JavaUtils.fieldExpression("this")(JavaUtils.javaIdentifier(fname)) });
  const compareToHeader = ({ tag: "complex", value: ({
    variant: compareToVar,
    typeArguments: [],
    identifier: JavaNames.compareToMethodName
  }) });
  const lhs = JavaUtils.javaRelationalExpressionToJavaEqualityExpression(JavaUtils.javaPostfixExpressionToJavaRelationalExpression(JavaUtils.javaMethodInvocationToJavaPostfixExpression(({
    header: compareToHeader,
    arguments: [compareToArg]
  }))));
  const rhs = JavaUtils.javaPostfixExpressionToJavaRelationalExpression(({ tag: "primary", value: JavaUtils.javaLiteralToJavaPrimary(JavaUtils.javaInt(0n)) }));
  return JavaUtils.javaEqualityExpressionToJavaInclusiveOrExpression(({ tag: "equal", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
})());
}

export function constantDecl(javaName: string): ((x: JavaEnvironment.Aliases) => ((x: Core.Name) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments)))) {
  return ((aliases: JavaEnvironment.Aliases) => ((name: Core.Name) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const mods = [({ tag: "public" }), ({ tag: "static" }), ({ tag: "final" })];
  const nameName = JavaUtils.nameToJavaName(aliases)("hydra.core.Name");
  return (() => {
  const env = ({
    aliases: aliases,
    graph: g
  });
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(({ tag: "variable", value: "hydra.core.Name" }))(cx)(g))(((jt: JavaSyntax.Type) => LibEithers.bind(encodeTerm(env)(({ tag: "literal", value: ({ tag: "string", value: ((_x) => _x)(name) }) }))(cx)(g))(((arg: JavaSyntax.Expression) => (() => {
  const init = ({ tag: "expression", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(nameName)(null))([arg])(null) });
  return (() => {
  const var_ = JavaUtils.javaVariableDeclarator(javaName)(init);
  return ({ tag: "right", value: noComment(JavaUtils.javaMemberField(mods)(jt)(var_)) });
})();
})()))));
})();
})()))));
}

export function constantDeclForFieldType(aliases: JavaEnvironment.Aliases): ((x: Core.FieldType) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments))) {
  return ((ftyp: Core.FieldType) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const name = ((_x) => _x.name)(ftyp);
  const javaName = Formatting.nonAlnumToUnderscores(Formatting.convertCase(({ tag: "camel" }))(({ tag: "upperSnake" }))(((_x) => _x)(name)));
  return constantDecl(javaName)(aliases)(name)(cx)(g);
})())));
}

export function constantDeclForTypeName(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments))) {
  return ((name: Core.Name) => ((cx: Context.Context) => ((g: Graph.Graph) => constantDecl("TYPE_")(aliases)(name)(cx)(g))));
}

export function constructElementsInterface(mod: Packaging.Module): ((x: ReadonlyArray<JavaSyntax.InterfaceMemberDeclaration>) => readonly [Core.Name, JavaSyntax.CompilationUnit]) {
  return ((members: ReadonlyArray<JavaSyntax.InterfaceMemberDeclaration>) => (() => {
  const ns = ((_x) => _x.namespace)(mod);
  const parentNs = namespaceParent(ns);
  const pkg = LibMaybes.cases(parentNs)(JavaUtils.javaPackageDeclaration(ns))(((pns: Packaging.Namespace) => JavaUtils.javaPackageDeclaration(pns)));
  const mods = [({ tag: "public" })];
  const className = elementsClassName(ns);
  const elName = elementsQualifiedName(ns);
  const body = members;
  const itf = ({ tag: "interface", value: ({ tag: "normalInterface", value: ({
    modifiers: mods,
    identifier: JavaUtils.javaTypeIdentifier(className),
    parameters: [],
    extends: [],
    body: body
  }) }) });
  const decl = ({
    value: itf,
    comments: ((_x) => _x.description)(mod)
  });
  return [elName, ({ tag: "ordinary", value: ({
    package: pkg,
    imports: [],
    types: [decl]
  }) })];
})());
}

export function correctCastType<t0, t1, t2>(innerBody: Core.Term): ((x: ReadonlyArray<Core.Type>) => ((x: Core.Type) => ((x: t0) => ((x: t1) => t2 | Core.Type)))) {
  return ((typeArgs: ReadonlyArray<Core.Type>) => ((fallback: Core.Type) => ((cx: t0) => ((g: t1) => (() => {
  const _m = Strip.deannotateTerm(innerBody);
  switch (_m.tag) {
    case "pair": return ((_p: readonly [Core.Term, Core.Term]) => LibLogic.ifElse(LibEquality.equal(LibLists.length(typeArgs))(2))(({ tag: "right", value: ({ tag: "pair", value: ({
    first: LibLists.head(typeArgs),
    second: LibLists.head(LibLists.tail(typeArgs))
  }) }) }))(({ tag: "right", value: fallback })))((_m as any).value);
    default: return ({ tag: "right", value: fallback })(_m);
  }
})()))));
}

export function correctTypeApps<t0, t1>(gr: t0): ((x: Core.Name) => ((x: ReadonlyArray<Core.Term>) => ((x: ReadonlyArray<Core.Type>) => ((x: t1) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<Core.Type>))))) {
  return ((name: Core.Name) => ((args: ReadonlyArray<Core.Term>) => ((fallbackTypeApps: ReadonlyArray<Core.Type>) => ((cx: t1) => ((g: Graph.Graph) => LibEithers.bind(({ tag: "right", value: Lexical.lookupBinding(g)(name) }))(((mel: Core.Binding | null) => LibMaybes.cases(mel)(({ tag: "right", value: fallbackTypeApps }))(((el: Core.Binding) => LibMaybes.cases(((_x) => _x.type)(el))(({ tag: "right", value: fallbackTypeApps }))(((ts: Core.TypeScheme) => (() => {
  const schemeType = ((_x) => _x.type)(ts);
  return (() => {
  const allSchemeVars = LibLists.filter(((v: Core.Name) => isSimpleName(v)))(((_x) => _x.variables)(ts));
  return (() => {
  const schemeTypeVars = collectTypeVars(schemeType);
  return (() => {
  const usedFlags = LibLists.map(((v: Core.Name) => LibSets.member(v)(schemeTypeVars)))(allSchemeVars);
  return (() => {
  const usedSchemeVars = filterByFlags(allSchemeVars)(usedFlags);
  return (() => {
  const nParams = countFunctionParams(schemeType);
  return (() => {
  const peeled = peelDomainTypes(nParams)(schemeType);
  return (() => {
  const calleeDoms = LibPairs.first(peeled);
  return (() => {
  const calleeCod = LibPairs.second(peeled);
  return (() => {
  const overgenSubst = detectAccumulatorUnification(calleeDoms)(calleeCod)(usedSchemeVars);
  return (() => {
  const keepFlags = LibLists.map(((v: Core.Name) => LibLogic.and(LibSets.member(v)(schemeTypeVars))(LibLogic.not(LibMaps.member(v)(overgenSubst)))))(allSchemeVars);
  return (() => {
  const schemeVars = filterByFlags(allSchemeVars)(keepFlags);
  return (() => {
  const filteredFallback0 = LibLogic.ifElse(LibEquality.equal(LibLists.length(allSchemeVars))(LibLists.length(fallbackTypeApps)))(filterByFlags(fallbackTypeApps)(keepFlags))(fallbackTypeApps);
  return (() => {
  const filteredFallback = LibLogic.ifElse(LibMaps.null_(overgenSubst))(filteredFallback0)(LibLists.map(((t: Core.Type) => substituteTypeVarsWithTypes(overgenSubst)(t)))(filteredFallback0));
  return LibLogic.ifElse(LibLogic.or(LibLists.null_(schemeVars))(LibLogic.not(LibEquality.equal(LibLists.length(schemeVars))(LibLists.length(filteredFallback)))))(({ tag: "right", value: filteredFallback }))(correctTypeAppsWithArgs(schemeVars)(filteredFallback)(schemeType)(args)(cx)(g));
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
})())))))))))));
}

export function correctTypeAppsWithArgs<t0>(schemeVars: ReadonlyArray<Core.Name>): ((x: ReadonlyArray<Core.Type>) => ((x: Core.Type) => ((x: ReadonlyArray<Core.Term>) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<Core.Type>))))) {
  return ((fallbackTypeApps: ReadonlyArray<Core.Type>) => ((schemeType: Core.Type) => ((args: ReadonlyArray<Core.Term>) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const schemeVarSet = LibSets.fromList(schemeVars);
  return (() => {
  const irSubst = LibMaps.fromList(LibLists.zip(schemeVars)(fallbackTypeApps));
  return (() => {
  const peeled = peelDomainTypes(LibLists.length(args))(schemeType);
  return (() => {
  const schemeDoms = LibPairs.first(peeled);
  return LibEithers.bind(LibEithers.mapList(((arg: Core.Term) => LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(Annotations.termAnnotationInternal(arg)))))(args))(((mArgTypes: ReadonlyArray<Core.Type | null>) => LibLogic.ifElse(LibLogic.not(LibLists.null_(LibLists.filter(((m: Core.Type | null) => LibMaybes.isNothing(m)))(mArgTypes))))(({ tag: "right", value: fallbackTypeApps }))((() => {
  const argTypes = LibLists.bind(mArgTypes)(((m: Core.Type | null) => LibMaybes.cases(m)([])(((x: Core.Type) => LibLists.pure(x)))));
  return (() => {
  const irDoms = LibLists.map(((d: Core.Type) => applySubstSimple(irSubst)(d)))(schemeDoms);
  return (() => {
  const domsMatch = LibLists.null_(LibLists.filter(((p: readonly [Core.Type, Core.Type]) => LibLogic.not(typesMatch(Strip.deannotateType(LibPairs.first(p)))(Strip.deannotateType(LibPairs.second(p))))))(LibLists.zip(irDoms)(argTypes)));
  return LibLogic.ifElse(domsMatch)(({ tag: "right", value: fallbackTypeApps }))(({ tag: "right", value: resolveTypeApps(schemeVars)(fallbackTypeApps)(buildArgSubst(schemeVarSet)(schemeDoms)(argTypes)) }));
})();
})();
})())));
})();
})();
})();
})())))));
}

export function countFunctionParams(t: Core.Type): number {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => LibMath.add(1)(countFunctionParams(((_x) => _x.codomain)(ft))))((_m as any).value);
    default: return 0(_m);
  }
})();
}

export function declarationForRecordType(isInner: boolean): ((x: boolean) => ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassDeclaration))))))) {
  return ((isSer: boolean) => ((aliases: JavaEnvironment.Aliases) => ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((cx: Context.Context) => ((g: Graph.Graph) => declarationForRecordType_(isInner)(isSer)(aliases)(tparams)(elName)(null)(fields)(cx)(g))))))));
}

export function declarationForRecordType_(isInner: boolean): ((x: boolean) => ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: Core.Name | null) => ((x: ReadonlyArray<Core.FieldType>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassDeclaration)))))))) {
  return ((isSer: boolean) => ((aliases: JavaEnvironment.Aliases) => ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((parentName: Core.Name | null) => ((fields: ReadonlyArray<Core.FieldType>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => recordMemberVar(aliases)(f)(cx)(g)))(fields))(((memberVars: ReadonlyArray<JavaSyntax.ClassBodyDeclaration>) => LibEithers.bind(LibEithers.mapList(((p: readonly [JavaSyntax.ClassBodyDeclaration, Core.FieldType]) => addComment(LibPairs.first(p))(LibPairs.second(p))(cx)(g)))(LibLists.zip(memberVars)(fields)))(((memberVars_: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => LibEithers.bind(LibLogic.ifElse(LibEquality.gt(LibLists.length(fields))(1))(LibEithers.mapList(((f: Core.FieldType) => recordWithMethod(aliases)(elName)(fields)(f)(cx)(g)))(fields))(({ tag: "right", value: [] })))(((withMethods: ReadonlyArray<JavaSyntax.ClassBodyDeclaration>) => LibEithers.bind(recordConstructor(aliases)(elName)(fields)(cx)(g))(((cons: JavaSyntax.ClassBodyDeclaration) => LibEithers.bind(LibLogic.ifElse(isInner)(({ tag: "right", value: [] }))(LibEithers.bind(constantDeclForTypeName(aliases)(elName)(cx)(g))(((d: JavaSyntax.ClassBodyDeclarationWithComments) => LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => constantDeclForFieldType(aliases)(f)(cx)(g)))(fields))(((dfields: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => ({ tag: "right", value: LibLists.cons(d)(dfields) })))))))(((tn: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => (() => {
  const comparableMethods = LibMaybes.cases(parentName)(LibLogic.ifElse(LibLogic.and(LibLogic.not(isInner))(isSer))([recordCompareToMethod(aliases)(tparams)(elName)(fields)])([]))(((pn: Core.Name) => LibLogic.ifElse(isSer)([variantCompareToMethod(aliases)(tparams)(pn)(elName)(fields)])([])));
  return (() => {
  const bodyDecls = LibLists.concat2(tn)(LibLists.concat2(memberVars_)(LibLists.map(((x: JavaSyntax.ClassBodyDeclaration) => noComment(x)))(LibLists.concat2([cons, recordEqualsMethod(aliases)(elName)(fields), recordHashCodeMethod(fields)])(LibLists.concat2(comparableMethods)(withMethods)))));
  return (() => {
  const ifaces = LibLogic.ifElse(isInner)(serializableTypes(isSer))(interfaceTypes(isSer)(aliases)(tparams)(elName));
  return ({ tag: "right", value: JavaUtils.javaClassDeclaration(aliases)(tparams)(elName)(classModsPublic)(null)(ifaces)(bodyDecls) });
})();
})();
})()))))))))))))))))));
}

export function declarationForUnionType(isSer: boolean): ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassDeclaration)))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(LibEithers.mapList(((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(ft);
  return (() => {
  const ftype = ((_x) => _x.type)(ft);
  return (() => {
  const rfields = LibLogic.ifElse(Predicates.isUnitType(Strip.deannotateType(ftype)))([])([({
    name: "value",
    type: Strip.deannotateType(ftype)
  })]);
  return (() => {
  const varName = JavaUtils.variantClassName(false)(elName)(fname);
  return LibEithers.bind(declarationForRecordType_(true)(isSer)(aliases)([])(varName)(LibLogic.ifElse(isSer)(elName)(null))(rfields)(cx)(g))(((innerDecl: JavaSyntax.ClassDeclaration) => ({ tag: "right", value: augmentVariantClass(aliases)(tparams)(elName)(innerDecl) })));
})();
})();
})();
})()))(fields))(((variantClasses: ReadonlyArray<JavaSyntax.ClassDeclaration>) => (() => {
  const variantDecls = LibLists.map(((vc: JavaSyntax.ClassDeclaration) => ({ tag: "classMember", value: ({ tag: "class", value: vc }) })))(variantClasses);
  return LibEithers.bind(LibEithers.mapList(((pair: readonly [JavaSyntax.ClassBodyDeclaration, Core.FieldType]) => addComment(LibPairs.first(pair))(LibPairs.second(pair))(cx)(g)))(LibLists.zip(variantDecls)(fields)))(((variantDecls_: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => (() => {
  const privateConst = JavaUtils.makeConstructor(aliases)(elName)(true)([])([]);
  return (() => {
  const acceptDecl = JavaUtils.toAcceptMethod(true)(tparams);
  return (() => {
  const vtparams = LibLists.concat2(tparams)([JavaUtils.javaTypeParameter(JavaNames.visitorReturnParameter)]);
  return (() => {
  const visitorMethods = LibLists.map(((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(ft);
  return (() => {
  const typeArgs = LibLists.map(((tp: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToTypeArgument(tp)))(tparams);
  return (() => {
  const varRef = JavaUtils.javaClassTypeToJavaType(JavaUtils.nameToJavaClassType(aliases)(false)(typeArgs)(JavaUtils.variantClassName(false)(elName)(fname))(null));
  return (() => {
  const param = JavaUtils.javaTypeToJavaFormalParameter(varRef)("instance");
  return (() => {
  const resultR = JavaUtils.javaTypeToJavaResult(({ tag: "reference", value: JavaUtils.visitorTypeVariable }));
  return JavaUtils.interfaceMethodDeclaration([])([])(JavaNames.visitMethodName)([param])(resultR)(null);
})();
})();
})();
})();
})()))(fields);
  return (() => {
  const visitorBody = visitorMethods;
  return (() => {
  const visitor = JavaUtils.javaInterfaceDeclarationToJavaClassBodyDeclaration(({
    modifiers: [({ tag: "public" })],
    identifier: JavaNames.visitorName,
    parameters: vtparams,
    extends: [],
    body: visitorBody
  }));
  return (() => {
  const typeArgs = LibLists.map(((tp: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToTypeArgument(tp)))(tparams);
  return (() => {
  const visitorClassType = JavaUtils.javaClassType(LibLists.concat2(LibLists.map(((tp: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToReferenceType(tp)))(tparams))([JavaUtils.visitorTypeVariable]))(null)(JavaNames.visitorName);
  return (() => {
  const mainInstanceParam = JavaUtils.javaTypeToJavaFormalParameter(JavaUtils.javaClassTypeToJavaType(JavaUtils.nameToJavaClassType(aliases)(false)(typeArgs)(elName)(null)))("instance");
  return (() => {
  const resultR = JavaUtils.javaTypeToJavaResult(({ tag: "reference", value: JavaUtils.visitorTypeVariable }));
  return (() => {
  const throwStmt = ({ tag: "statement", value: JavaUtils.javaThrowIllegalStateException([JavaUtils.javaAdditiveExpressionToJavaExpression(JavaUtils.addExpressions([JavaUtils.javaStringMultiplicativeExpression("Non-exhaustive patterns when matching: "), ({ tag: "unary", value: JavaUtils.javaIdentifierToJavaUnaryExpression("instance") })]))]) });
  return (() => {
  const defaultMod = [({ tag: "default" })];
  return (() => {
  const otherwiseDecl = JavaUtils.interfaceMethodDeclaration(defaultMod)([])(JavaNames.otherwiseMethodName)([mainInstanceParam])(resultR)([throwStmt]);
  return (() => {
  const pvVisitMethods = LibLists.map(((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x.name)(ft);
  return (() => {
  const varRef = JavaUtils.javaClassTypeToJavaType(JavaUtils.nameToJavaClassType(aliases)(false)(typeArgs)(JavaUtils.variantClassName(false)(elName)(fname))(null));
  return (() => {
  const param = JavaUtils.javaTypeToJavaFormalParameter(varRef)("instance");
  return (() => {
  const mi = JavaUtils.methodInvocation(null)(JavaNames.otherwiseMethodName)([JavaUtils.javaIdentifierToJavaExpression("instance")]);
  return (() => {
  const returnOtherwise = ({ tag: "statement", value: JavaUtils.javaReturnStatement(JavaUtils.javaPrimaryToJavaExpression(JavaUtils.javaMethodInvocationToJavaPrimary(mi))) });
  return JavaUtils.interfaceMethodDeclaration(defaultMod)([])(JavaNames.visitMethodName)([param])(resultR)([returnOtherwise]);
})();
})();
})();
})();
})()))(fields);
  return (() => {
  const pvBody = LibLists.concat2([otherwiseDecl])(pvVisitMethods);
  return (() => {
  const partialVisitor = JavaUtils.javaInterfaceDeclarationToJavaClassBodyDeclaration(({
    modifiers: [({ tag: "public" })],
    identifier: JavaNames.partialVisitorName,
    parameters: vtparams,
    extends: [visitorClassType],
    body: pvBody
  }));
  return LibEithers.bind(constantDeclForTypeName(aliases)(elName)(cx)(g))(((tn0: JavaSyntax.ClassBodyDeclarationWithComments) => LibEithers.bind(LibEithers.mapList(((ft: Core.FieldType) => constantDeclForFieldType(aliases)(ft)(cx)(g)))(fields))(((tn1: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => (() => {
  const tn = LibLists.concat2([tn0])(tn1);
  return (() => {
  const otherDecls = LibLists.map(((d: JavaSyntax.ClassBodyDeclaration) => noComment(d)))([privateConst, acceptDecl, visitor, partialVisitor]);
  return (() => {
  const bodyDecls = LibLists.concat([tn, otherDecls, variantDecls_]);
  return (() => {
  const mods = LibLists.concat2(classModsPublic)([({ tag: "abstract" })]);
  return ({ tag: "right", value: JavaUtils.javaClassDeclaration(aliases)(tparams)(elName)(mods)(null)(interfaceTypes(isSer)(aliases)(tparams)(elName))(bodyDecls) });
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
})();
})();
})();
})();
})();
})();
})()));
})()))))))));
}

export function decodeTypeFromTerm(term: Core.Term): Core.Type | null {
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "inject": return ((inj: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x.typeName)(inj))("hydra.core.Type"))((() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(((_x) => _x.field)(inj)));
  return (() => {
  const fterm = ((_x) => _x.term)(((_x) => _x.field)(inj));
  return LibLogic.ifElse(LibEquality.equal(fname)("variable"))((() => {
  const _m = fterm;
  switch (_m.tag) {
    case "wrap": return ((wt: Core.WrappedTerm) => (() => {
  const _m = ((_x) => _x.body)(wt);
  switch (_m.tag) {
    case "literal": return ((lit: Core.Literal) => (() => {
  const _m = lit;
  switch (_m.tag) {
    case "string": return ((s: string) => ({ tag: "variable", value: s }))((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})())(LibLogic.ifElse(LibEquality.equal(fname)("annotated"))((() => {
  const _m = fterm;
  switch (_m.tag) {
    case "record": return ((rec: Core.Record) => LibMaybes.bind(LibLists.safeHead(LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))("body")))(((_x) => _x.fields)(rec))))(((bodyField: Core.Field) => decodeTypeFromTerm(((_x) => _x.term)(bodyField)))))((_m as any).value);
    default: return null(_m);
  }
})())(LibLogic.ifElse(LibEquality.equal(fname)("application"))((() => {
  const _m = fterm;
  switch (_m.tag) {
    case "record": return ((rec: Core.Record) => LibMaybes.bind(LibLists.safeHead(LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))("function")))(((_x) => _x.fields)(rec))))(((funcField: Core.Field) => LibMaybes.bind(decodeTypeFromTerm(((_x) => _x.term)(funcField)))(((func: Core.Type) => LibMaybes.bind(LibLists.safeHead(LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))("argument")))(((_x) => _x.fields)(rec))))(((argField: Core.Field) => LibMaybes.map(((arg: Core.Type) => ({ tag: "application", value: ({
    function: func,
    argument: arg
  }) })))(decodeTypeFromTerm(((_x) => _x.term)(argField))))))))))((_m as any).value);
    default: return null(_m);
  }
})())(LibLogic.ifElse(LibEquality.equal(fname)("function"))((() => {
  const _m = fterm;
  switch (_m.tag) {
    case "record": return ((rec: Core.Record) => LibMaybes.bind(LibLists.safeHead(LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))("domain")))(((_x) => _x.fields)(rec))))(((domField: Core.Field) => LibMaybes.bind(decodeTypeFromTerm(((_x) => _x.term)(domField)))(((dom: Core.Type) => LibMaybes.bind(LibLists.safeHead(LibLists.filter(((f: Core.Field) => LibEquality.equal(((_x) => _x.name)(f))("codomain")))(((_x) => _x.fields)(rec))))(((codField: Core.Field) => LibMaybes.map(((cod: Core.Type) => ({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) })))(decodeTypeFromTerm(((_x) => _x.term)(codField))))))))))((_m as any).value);
    default: return null(_m);
  }
})())(LibLogic.ifElse(LibEquality.equal(fname)("literal"))((() => {
  const _m = fterm;
  switch (_m.tag) {
    case "inject": return ((litInj: Core.Injection) => LibLogic.ifElse(LibEquality.equal(((_x) => _x)(((_x) => _x.name)(((_x) => _x.field)(litInj))))("string"))(({ tag: "literal", value: ({ tag: "string" }) }))(null))((_m as any).value);
    default: return null(_m);
  }
})())(null)))));
})();
})())(null))((_m as any).value);
    default: return null(_m);
  }
})();
}

export function dedupBindings(inScope: ReadonlySet<Core.Name>): ((x: ReadonlyArray<Core.Binding>) => ReadonlyArray<Core.Binding>) {
  return ((bs: ReadonlyArray<Core.Binding>) => LibLogic.ifElse(LibLists.null_(bs))([])((() => {
  const b = LibLists.head(bs);
  return (() => {
  const rest = LibLists.tail(bs);
  return (() => {
  const name = ((_x) => _x.name)(b);
  return LibLogic.ifElse(LibSets.member(name)(inScope))((() => {
  const newName = freshJavaName(name)(inScope);
  return (() => {
  const subst = LibMaps.singleton(name)(newName);
  return (() => {
  const rest2 = LibLists.map(((b2: Core.Binding) => ({
    name: ((_x) => _x.name)(b2),
    term: Variables.substituteVariables(subst)(((_x) => _x.term)(b2)),
    type: ((_x) => _x.type)(b2)
  })))(rest);
  return LibLists.cons(({
    name: newName,
    term: ((_x) => _x.term)(b),
    type: ((_x) => _x.type)(b)
  }))(dedupBindings(LibSets.insert(newName)(inScope))(rest2));
})();
})();
})())(LibLists.cons(b)(dedupBindings(LibSets.insert(name)(inScope))(rest)));
})();
})();
})()));
}

export function detectAccumulatorUnification(doms: ReadonlyArray<Core.Type>): ((x: Core.Type) => ((x: ReadonlyArray<Core.Name>) => ReadonlyMap<Core.Name, Core.Type>)) {
  return ((cod: Core.Type) => ((tparams: ReadonlyArray<Core.Name>) => (() => {
  const tparamSet = LibSets.fromList(tparams);
  return (() => {
  const allPairs = LibLists.bind(doms)(((d: Core.Type) => extractInOutPair(d)));
  return (() => {
  const groupedByInput = groupPairsByFirst(allPairs);
  return (() => {
  const selfRefSubst = selfRefSubstitution(groupedByInput);
  return (() => {
  const directPairs = LibLists.bind(doms)(((d: Core.Type) => extractDirectReturn(tparamSet)(d)));
  return (() => {
  const groupedDirect = groupPairsByFirst(directPairs);
  return (() => {
  const directInputVars = LibSets.fromList(LibLists.map(((p: readonly [Core.Name, Core.Name]) => LibPairs.first(p)))(directPairs));
  return (() => {
  const codVar = (() => {
  const _m = Strip.deannotateType(cod);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => v)((_m as any).value);
    default: return null(_m);
  }
})();
  return (() => {
  const directRefSubst = directRefSubstitution(directInputVars)(codVar)(groupedDirect);
  return (() => {
  const codSubst = LibMaybes.maybe(LibMaps.empty)(((cv: Core.Name) => LibLogic.ifElse(LibMaps.member(cv)(selfRefSubst))(LibMaps.empty)(LibMaybes.maybe(LibMaps.empty)(((refVar: Core.Name) => LibLogic.ifElse(LibEquality.equal(cv)(refVar))(LibMaps.empty)(LibMaps.singleton(cv)(refVar))))(findSelfRefVar(groupedByInput)))))(findPairFirst(cod));
  return (() => {
  const domVars = LibSets.fromList(LibLists.bind(doms)(((d: Core.Type) => LibSets.toList(collectTypeVars(d)))));
  return (() => {
  const danglingSubst = LibMaybes.maybe(LibMaps.empty)(((cv: Core.Name) => LibLogic.ifElse(LibSets.member(cv)(domVars))(LibMaps.empty)(LibMaybes.maybe(LibMaps.empty)(((refVar: Core.Name) => LibMaps.singleton(cv)(({ tag: "variable", value: refVar }))))(findSelfRefVar(groupedByInput)))))(findPairFirst(cod));
  return LibMaps.union(LibMaps.union(LibMaps.union(nameMapToTypeMap(selfRefSubst))(nameMapToTypeMap(codSubst)))(danglingSubst))(nameMapToTypeMap(directRefSubst));
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
})()));
}

export function directRefSubstitution<t0>(directInputVars: ReadonlySet<t0>): ((x: t0 | null) => ((x: ReadonlyMap<t0, ReadonlyArray<t0>>) => ReadonlyMap<t0, t0>)) {
  return ((codVar: t0 | null) => ((grouped: ReadonlyMap<t0, ReadonlyArray<t0>>) => LibLists.foldl(((subst: ReadonlyMap<t0, t0>) => ((entry: readonly [t0, ReadonlyArray<t0>]) => directRefSubstitution_processGroup(directInputVars)(codVar)(subst)(LibPairs.first(entry))(LibPairs.second(entry)))))(LibMaps.empty)(LibMaps.toList(grouped))));
}

export function directRefSubstitution_processGroup<t0>(directInputVars: ReadonlySet<t0>): ((x: t0 | null) => ((x: ReadonlyMap<t0, t0>) => ((x: t0) => ((x: ReadonlyArray<t0>) => ReadonlyMap<t0, t0>)))) {
  return ((codVar: t0 | null) => ((subst: ReadonlyMap<t0, t0>) => ((inVar: t0) => ((outVars: ReadonlyArray<t0>) => (() => {
  const selfRefCount = LibLists.length(LibLists.filter(((v: t0) => LibEquality.equal(v)(inVar)))(outVars));
  return (() => {
  const nonSelfVars = LibLists.filter(((v: t0) => LibLogic.not(LibEquality.equal(v)(inVar))))(outVars);
  return (() => {
  const safeNonSelfVars = LibLists.filter(((v: t0) => LibLogic.and(LibLogic.not(LibSets.member(v)(directInputVars)))(LibLogic.not(LibEquality.equal(v)(codVar)))))(nonSelfVars);
  return LibLogic.ifElse(LibLogic.and(LibEquality.gte(selfRefCount)(2))(LibLogic.not(LibLists.null_(safeNonSelfVars))))(LibLists.foldl(((s: ReadonlyMap<t0, t0>) => ((v: t0) => LibMaps.insert(v)(inVar)(s))))(subst)(safeNonSelfVars))(subst);
})();
})();
})()))));
}

export function domTypeArgs<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Type) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<JavaSyntax.TypeArgument>))) {
  return ((d: Core.Type) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const args = extractTypeApplicationArgs(Strip.deannotateType(d));
  return LibLogic.ifElse(LibLogic.not(LibLists.null_(args)))(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: ({ tag: "reference", value: rt }) })))))))(args))(({ tag: "right", value: javaTypeArgumentsForType(d) }));
})())));
}

export function elementJavaIdentifier(isPrim: boolean): ((x: boolean) => ((x: JavaEnvironment.Aliases) => ((x: Core.Name) => JavaSyntax.Identifier))) {
  return ((isMethod: boolean) => ((aliases: JavaEnvironment.Aliases) => ((name: Core.Name) => (() => {
  const qn = Names.qualifyName(name);
  const ns_ = ((_x) => _x.namespace)(qn);
  const local = ((_x) => _x.local)(qn);
  const sep = LibLogic.ifElse(isMethod)("::")(".");
  return LibLogic.ifElse(isPrim)(LibStrings.cat2(LibStrings.cat2(elementJavaIdentifier_qualify(aliases)(ns_)(Formatting.capitalize(local)))("."))(JavaNames.applyMethodName))(LibMaybes.cases(ns_)(JavaUtils.sanitizeJavaName(local))(((n: Packaging.Namespace) => LibStrings.cat2(LibStrings.cat2(elementJavaIdentifier_qualify(aliases)(namespaceParent(n))(elementsClassName(n)))(sep))(JavaUtils.sanitizeJavaName(local)))));
})())));
}

export function elementJavaIdentifier_qualify(aliases: JavaEnvironment.Aliases): ((x: Packaging.Namespace | null) => ((x: string) => string)) {
  return ((mns: Packaging.Namespace | null) => ((s: string) => ((_x) => _x)(JavaUtils.nameToJavaName(aliases)(Names.unqualifyName(({
    namespace: mns,
    local: s
  }))))));
}

export function elementsClassName(ns: Packaging.Namespace): string {
  return (() => {
  const nsStr = ((_x) => _x)(ns);
  const parts = LibStrings.splitOn(".")(nsStr);
  return Formatting.sanitizeWithUnderscores(JavaLanguage.reservedWords)(Formatting.capitalize(LibLists.last(parts)));
})();
}

export function elementsQualifiedName(ns: Packaging.Namespace): Core.Name {
  return Names.unqualifyName(({
    namespace: namespaceParent(ns),
    local: elementsClassName(ns)
  }));
}

export function encodeApplication(env: JavaEnvironment.JavaEnvironment): ((x: Core.Application) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))) {
  return ((app: Core.Application) => ((cx: Context.Context) => ((g0: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const g = ((_x) => _x.graph)(env);
  return (() => {
  const gathered = Analysis.gatherArgsWithTypeApps(({ tag: "application", value: app }))([])([]);
  return (() => {
  const fun = LibPairs.first(gathered);
  return (() => {
  const args = LibPairs.first(LibPairs.second(gathered));
  return (() => {
  const typeApps = LibPairs.second(LibPairs.second(gathered));
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(Annotations.termAnnotationInternal(fun))))(((mfunTyp: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mfunTyp)(Checking.typeOfTerm(cx)(g)(fun))(((t: Core.Type) => ({ tag: "right", value: t }))))(((funTyp: Core.Type) => (() => {
  const arity = Arity.typeArity(funTyp);
  return (() => {
  const deannotatedFun = Strip.deannotateTerm(fun);
  return (() => {
  const calleeName = (() => {
  const _m = deannotatedFun;
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => n)((_m as any).value);
    default: return null(_m);
  }
})();
  return LibEithers.bind(LibMaybes.cases(calleeName)(({ tag: "right", value: args }))(((cname: Core.Name) => annotateLambdaArgs(cname)(typeApps)(args)(cx)(g))))(((annotatedArgs: ReadonlyArray<Core.Term>) => (() => {
  const _m = deannotatedFun;
  switch (_m.tag) {
    case "variable": return ((name: Core.Name) => LibLogic.ifElse(LibMaybes.isJust(LibMaps.lookup(name)(((_x) => _x.primitives)(g))))((() => {
  const hargs = LibLists.take(arity)(annotatedArgs);
  return (() => {
  const rargs = LibLists.drop(arity)(annotatedArgs);
  return LibEithers.bind(functionCall(env)(true)(name)(hargs)([])(cx)(g))(((initialCall: JavaSyntax.Expression) => LibEithers.foldl(((acc: JavaSyntax.Expression) => ((h: Core.Term) => LibEithers.bind(encodeTerm(env)(h)(cx)(g))(((jarg: JavaSyntax.Expression) => ({ tag: "right", value: applyJavaArg(acc)(jarg) }))))))(initialCall)(rargs)));
})();
})())(LibLogic.ifElse(LibLogic.and(isRecursiveVariable(aliases)(name))(LibLogic.not(isLambdaBoundIn(name)(((_x) => _x.lambdaVars)(aliases)))))(encodeApplication_fallback(env)(aliases)(g)(typeApps)(((_x) => _x.function)(app))(((_x) => _x.argument)(app))(cx)(g))(LibEithers.bind(classifyDataReference(name)(cx)(g))(((symClass: JavaEnvironment.JavaSymbolClass) => (() => {
  const methodArity = (() => {
  const _m = symClass;
  switch (_m.tag) {
    case "hoistedLambda": return ((n: number) => n)((_m as any).value);
    default: return arity(_m);
  }
})();
  return (() => {
  const hargs = LibLists.take(methodArity)(annotatedArgs);
  return (() => {
  const rargs = LibLists.drop(methodArity)(annotatedArgs);
  return (() => {
  const trusted = ((_x) => _x.trustedTypeVars)(aliases);
  return (() => {
  const inScope = ((_x) => _x.inScopeTypeParams)(aliases);
  return (() => {
  const filteredTypeApps = LibLogic.ifElse(LibLogic.or(LibSets.null_(trusted))(LibSets.null_(inScope)))([])((() => {
  const allVars = LibSets.unions(LibLists.map(((t: Core.Type) => collectTypeVars(t)))(typeApps));
  return LibLogic.ifElse(LibLogic.not(LibSets.null_(LibSets.difference(allVars)(inScope))))([])(LibLogic.ifElse(LibSets.null_(LibSets.difference(allVars)(trusted)))(typeApps)([]));
})());
  return LibEithers.bind(LibLogic.ifElse(LibLists.null_(filteredTypeApps))(({ tag: "right", value: [] }))(correctTypeApps(g)(name)(hargs)(filteredTypeApps)(cx)(g)))(((safeTypeApps: ReadonlyArray<Core.Type>) => LibEithers.bind(filterPhantomTypeArgs(name)(safeTypeApps)(cx)(g))(((finalTypeApps: ReadonlyArray<Core.Type>) => LibEithers.bind(functionCall(env)(false)(name)(hargs)(finalTypeApps)(cx)(g))(((initialCall: JavaSyntax.Expression) => LibEithers.foldl(((acc: JavaSyntax.Expression) => ((h: Core.Term) => LibEithers.bind(encodeTerm(env)(h)(cx)(g))(((jarg: JavaSyntax.Expression) => ({ tag: "right", value: applyJavaArg(acc)(jarg) }))))))(initialCall)(rargs)))))));
})();
})();
})();
})();
})();
})())))))((_m as any).value);
    default: return encodeApplication_fallback(env)(aliases)(g)(typeApps)(((_x) => _x.function)(app))(((_x) => _x.argument)(app))(cx)(g)(_m);
  }
})()));
})();
})();
})()))));
})();
})();
})();
})();
})();
})())));
}

export function encodeApplication_fallback(env: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: Graph.Graph) => ((x: ReadonlyArray<Core.Type>) => ((x: Core.Term) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((gr: Graph.Graph) => ((typeApps: ReadonlyArray<Core.Type>) => ((lhs: Core.Term) => ((rhs: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(Annotations.termAnnotationInternal(lhs))))(((mt: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mt)(Checking.typeOfTerm(cx)(g)(lhs))(((typ: Core.Type) => ({ tag: "right", value: typ }))))(((t: Core.Type) => (() => {
  const _m = Strip.deannotateTypeParameters(Strip.deannotateType(t));
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = ((_x) => _x.domain)(ft);
  return (() => {
  const cod = ((_x) => _x.codomain)(ft);
  return (() => {
  const defaultExpr = LibEithers.bind(encodeTerm(env)(lhs)(cx)(g))(((jfun: JavaSyntax.Expression) => LibEithers.bind(encodeTerm(env)(rhs)(cx)(g))(((jarg: JavaSyntax.Expression) => ({ tag: "right", value: applyJavaArg(jfun)(jarg) })))));
  return (() => {
  const elimBranch = LibEithers.bind(encodeTerm(env)(rhs)(cx)(g))(((jarg: JavaSyntax.Expression) => LibEithers.bind(LibLogic.ifElse(LibLogic.not(LibLists.null_(javaTypeArgumentsForType(dom))))(({ tag: "right", value: dom }))(LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(Annotations.termAnnotationInternal(rhs))))(((mrt: Core.Type | null) => LibMaybes.cases(mrt)(LibEithers.bind(Checking.typeOfTerm(cx)(g)(rhs))(((rt: Core.Type) => ({ tag: "right", value: LibLogic.ifElse(LibLogic.not(LibLists.null_(javaTypeArgumentsForType(rt))))(rt)(dom) }))))(((rt: Core.Type) => ({ tag: "right", value: LibLogic.ifElse(LibLogic.not(LibLists.null_(javaTypeArgumentsForType(rt))))(rt)(dom) })))))))(((enrichedDom: Core.Type) => encodeElimination(env)(jarg)(enrichedDom)(cod)(Strip.deannotateTerm(lhs))(cx)(g)))));
  return (() => {
  const _m = Strip.deannotateTerm(lhs);
  switch (_m.tag) {
    case "project": return ((_p: Core.Projection) => elimBranch)((_m as any).value);
    case "cases": return ((_c: Core.CaseStatement) => elimBranch)((_m as any).value);
    case "unwrap": return ((_w: Core.Name) => elimBranch)((_m as any).value);
    default: return defaultExpr(_m);
  }
})();
})();
})();
})();
})())((_m as any).value);
    default: return LibEithers.bind(encodeTerm(env)(lhs)(cx)(g))(((jfun: JavaSyntax.Expression) => LibEithers.bind(encodeTerm(env)(rhs)(cx)(g))(((jarg: JavaSyntax.Expression) => ({ tag: "right", value: applyJavaArg(jfun)(jarg) })))))(_m);
  }
})())))))))))));
}

export function encodeDefinitions(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<Core.Name, JavaSyntax.CompilationUnit>))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const aliases = JavaUtils.importAliasesForModule(mod);
  return (() => {
  const env = ({
    aliases: aliases,
    graph: g
  });
  return (() => {
  const pkg = JavaUtils.javaPackageDeclaration(((_x) => _x.namespace)(mod));
  return (() => {
  const partitioned = Environment.partitionDefinitions(defs);
  return (() => {
  const typeDefs = LibPairs.first(partitioned);
  return (() => {
  const termDefs = LibPairs.second(partitioned);
  return (() => {
  const nonTypedefDefs = LibLists.filter(((td: Packaging.TypeDefinition) => (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(td));
  return isSerializableJavaType(typ);
})()))(typeDefs);
  return LibEithers.bind(LibEithers.mapList(((td: Packaging.TypeDefinition) => encodeTypeDefinition(pkg)(aliases)(td)(cx)(g)))(nonTypedefDefs))(((typeUnits: ReadonlyArray<readonly [Core.Name, JavaSyntax.CompilationUnit]>) => LibEithers.bind(LibLogic.ifElse(LibLists.null_(termDefs))(({ tag: "right", value: [] }))(LibEithers.bind(LibEithers.mapList(((td: Packaging.TermDefinition) => encodeTermDefinition(env)(td)(cx)(g)))(termDefs))(((dataMembers: ReadonlyArray<JavaSyntax.InterfaceMemberDeclaration>) => ({ tag: "right", value: [constructElementsInterface(mod)(dataMembers)] })))))(((termUnits: ReadonlyArray<readonly [Core.Name, JavaSyntax.CompilationUnit]>) => ({ tag: "right", value: LibMaps.fromList(LibLists.concat2(typeUnits)(termUnits)) })))));
})();
})();
})();
})();
})();
})();
})())));
}

export function encodeElimination(env: JavaEnvironment.JavaEnvironment): ((x: JavaSyntax.Expression | null) => ((x: Core.Type) => ((x: Core.Type) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))))) {
  return ((marg: JavaSyntax.Expression | null) => ((dom: Core.Type) => ((cod: Core.Type) => ((elimTerm: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const _m = Strip.deannotateTerm(elimTerm);
  switch (_m.tag) {
    case "project": return ((proj: Core.Projection) => (() => {
  const fname = ((_x) => _x.field)(proj);
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(dom)(cx)(g))(((jdom0: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jdom0)(cx))(((jdomr: JavaSyntax.ReferenceType) => LibMaybes.cases(marg)((() => {
  const projVar = "projected";
  return (() => {
  const jbody = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.variableToJavaIdentifier(projVar))(JavaUtils.javaIdentifier(((_x) => _x)(fname))));
  return ({ tag: "right", value: JavaUtils.javaLambda(projVar)(jbody) });
})();
})())(((jarg: JavaSyntax.Expression) => (() => {
  const qual = ({ tag: "primary", value: JavaUtils.javaExpressionToJavaPrimary(jarg) });
  return ({ tag: "right", value: JavaUtils.javaFieldAccessToJavaExpression(({
    qualifier: qual,
    identifier: JavaUtils.javaIdentifier(((_x) => _x)(fname))
  })) });
})()))))));
})())((_m as any).value);
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const def_ = ((_x) => _x.default)(cs);
  return (() => {
  const fields = ((_x) => _x.cases)(cs);
  return LibMaybes.cases(marg)((() => {
  const uVar = "u";
  return (() => {
  const typedLambda = ({ tag: "lambda", value: ({
    parameter: uVar,
    domain: dom,
    body: ({ tag: "application", value: ({
    function: elimTerm,
    argument: ({ tag: "variable", value: uVar })
  }) })
  }) });
  return encodeTerm(env)(typedLambda)(cx)(g);
})();
})())(((jarg: JavaSyntax.Expression) => (() => {
  const prim = JavaUtils.javaExpressionToJavaPrimary(jarg);
  return (() => {
  const consId = innerClassRef(aliases)(tname)(JavaNames.partialVisitorName);
  return (() => {
  const effectiveCod = cod;
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(effectiveCod)(cx)(g))(((jcod: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jcod)(cx))(((rt: JavaSyntax.ReferenceType) => LibEithers.bind(domTypeArgs(aliases)(dom)(cx)(g))(((domArgs: ReadonlyArray<JavaSyntax.TypeArgument>) => (() => {
  const targs = typeArgsOrDiamond(LibLists.concat2(domArgs)([({ tag: "reference", value: rt })]));
  return LibEithers.bind(LibMaybes.cases(def_)(({ tag: "right", value: [] }))(((d: Core.Term) => LibEithers.bind(otherwiseBranch(env)(aliases)(dom)(cod)(tname)(jcod)(domArgs)(d)(cx)(g))(((b: JavaSyntax.ClassBodyDeclarationWithComments) => ({ tag: "right", value: [b] }))))))(((otherwiseBranches: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => LibEithers.bind(LibEithers.mapList(((f: Core.Field) => visitBranch(env)(aliases)(dom)(tname)(jcod)(domArgs)(f)(cx)(g)))(fields))(((visitBranches: ReadonlyArray<JavaSyntax.ClassBodyDeclarationWithComments>) => (() => {
  const body = LibLists.concat2(otherwiseBranches)(visitBranches);
  return (() => {
  const visitor = JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(consId)(targs))([])(body);
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(({ tag: "right", value: prim }))(JavaNames.acceptMethodName)([visitor])) });
})();
})()))));
})()))))));
})();
})();
})()));
})();
})();
})())((_m as any).value);
    case "unwrap": return ((wrapName: Core.Name) => (() => {
  const withArg = ((ja: JavaSyntax.Expression) => JavaUtils.javaFieldAccessToJavaExpression(({
    qualifier: ({ tag: "primary", value: JavaUtils.javaExpressionToJavaPrimary(ja) }),
    identifier: JavaUtils.javaIdentifier(JavaNames.valueFieldName)
  })));
  return ({ tag: "right", value: LibMaybes.cases(marg)((() => {
  const wVar = "wrapped";
  return (() => {
  const wArg = JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(wVar));
  return JavaUtils.javaLambda(wVar)(withArg(wArg));
})();
})())(((jarg: JavaSyntax.Expression) => withArg(jarg))) });
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("unexpected ")(LibStrings.cat2("elimination case")(LibStrings.cat2(" in ")("encodeElimination"))) }) })(_m);
  }
})();
})()))))));
}

export function encodeFunction(env: JavaEnvironment.JavaEnvironment): ((x: Core.Type) => ((x: Core.Type) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))))) {
  return ((dom: Core.Type) => ((cod: Core.Type) => ((funTerm: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const encodeLambdaFallback = ((env2: JavaEnvironment.JavaEnvironment) => ((lam: Core.Lambda) => (() => {
  const lambdaVar = ((_x) => _x.parameter)(lam);
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return LibEithers.bind(analyzeJavaFunction(env2)(body)(cx)(g))(((fs: Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>) => (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const innerBody = ((_x) => _x.body)(fs);
  return (() => {
  const env3 = ((_x) => _x.environment)(fs);
  return LibEithers.bind(bindingsToStatements(env3)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env4 = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTerm(env4)(innerBody)(cx)(g))(((jbody: JavaSyntax.Expression) => (() => {
  const lam1 = LibLogic.ifElse(LibLists.null_(bindings))(JavaUtils.javaLambda(lambdaVar)(jbody))((() => {
  const returnSt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jbody) });
  return JavaUtils.javaLambdaFromBlock(lambdaVar)(LibLists.concat2(bindingStmts)([returnSt]));
})());
  return applyCastIfSafe(aliases)(({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }))(lam1)(cx)(g);
})()));
})();
})()));
})();
})();
})()));
})();
})()));
  return (() => {
  const _m = Strip.deannotateTerm(funTerm);
  switch (_m.tag) {
    case "project": return ((_p: Core.Projection) => encodeElimination(env)(null)(dom)(cod)(Strip.deannotateTerm(funTerm))(cx)(g))((_m as any).value);
    case "cases": return ((_c: Core.CaseStatement) => encodeElimination(env)(null)(dom)(cod)(Strip.deannotateTerm(funTerm))(cx)(g))((_m as any).value);
    case "unwrap": return ((_w: Core.Name) => encodeElimination(env)(null)(dom)(cod)(Strip.deannotateTerm(funTerm))(cx)(g))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => withLambda(env)(lam)(((env2: JavaEnvironment.JavaEnvironment) => (() => {
  const lambdaVar = ((_x) => _x.parameter)(lam);
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return (() => {
  const _m = Strip.deannotateTerm(body);
  switch (_m.tag) {
    case "lambda": return ((innerLam: Core.Lambda) => (() => {
  const _m = Strip.deannotateType(cod);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom2 = ((_x) => _x.domain)(ft);
  return (() => {
  const cod2 = ((_x) => _x.codomain)(ft);
  return LibEithers.bind(encodeFunction(env2)(dom2)(cod2)(({ tag: "lambda", value: innerLam }))(cx)(g))(((innerJavaLambda: JavaSyntax.Expression) => (() => {
  const lam1 = JavaUtils.javaLambda(lambdaVar)(innerJavaLambda);
  return applyCastIfSafe(aliases)(({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }))(lam1)(cx)(g);
})()));
})();
})())((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("expected function type for lambda body, but got: ")(ShowCore.type(cod)) }) })(_m);
  }
})())((_m as any).value);
    default: return encodeLambdaFallback(env2)(lam)(_m);
  }
})();
})();
})())))((_m as any).value);
    default: return ({ tag: "right", value: encodeLiteral(({ tag: "string", value: LibStrings.cat2("Unimplemented function variant: ")(ShowCore.term(funTerm)) })) })(_m);
  }
})();
})();
})())))));
}

export function encodeFunctionFormTerm(env: JavaEnvironment.JavaEnvironment): ((x: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))) {
  return ((anns: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((term: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mt: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mt)(LibMaybes.cases(tryInferFunctionType(term))(Checking.typeOfTerm(cx)(g)(term))(((inferredType: Core.Type) => ({ tag: "right", value: inferredType }))))(((t: Core.Type) => ({ tag: "right", value: t }))))(((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => encodeFunction(env)(((_x) => _x.domain)(ft))(((_x) => _x.codomain)(ft))(term)(cx)(g))((_m as any).value);
    default: return encodeNullaryConstant(env)(typ)(term)(cx)(g)(_m);
  }
})()))));
})()))));
}

export function encodeFunctionPrimitiveByName<t0>(env: JavaEnvironment.JavaEnvironment): ((x: Core.Type) => ((x: Core.Type) => ((x: Core.Name) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))))) {
  return ((dom: Core.Type) => ((cod: Core.Type) => ((name: Core.Name) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const classWithApply = ((_x) => _x)(elementJavaIdentifier(true)(false)(aliases)(name));
  return (() => {
  const suffix = LibStrings.cat2(".")(JavaNames.applyMethodName);
  return (() => {
  const className = LibStrings.fromList(LibLists.take(LibMath.sub(LibStrings.length(classWithApply))(LibStrings.length(suffix)))(LibStrings.toList(classWithApply)));
  return (() => {
  const arity = Arity.typeArity(({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }));
  return LibLogic.ifElse(LibEquality.lte(arity)(1))(({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(LibStrings.cat([className, "::", JavaNames.applyMethodName])) }))((() => {
  const paramNames = LibLists.map(((i: number) => LibStrings.cat2("p")(LibLiterals.showInt32(i))))(LibMath.range(0)(LibMath.sub(arity)(1)));
  return (() => {
  const paramExprs = LibLists.map(((p: Core.Name) => JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(p))))(paramNames);
  return (() => {
  const classId = className;
  return (() => {
  const call = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic(classId)(JavaNames.applyMethodName)(paramExprs));
  return (() => {
  const curried = buildCurriedLambda(paramNames)(call);
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) }))(cx)(g))(((jtype: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(rt)(JavaUtils.javaExpressionToJavaUnaryExpression(curried))) })))));
})();
})();
})();
})();
})());
})();
})();
})();
})();
})())))));
}

export function encodeLiteral(lit: Core.Literal): JavaSyntax.Expression {
  return (() => {
  const _m = lit;
  switch (_m.tag) {
    case "binary": return ((bs: Uint8Array) => (() => {
  const byteValues = LibLiterals.binaryToBytes(bs);
  return JavaUtils.javaArrayCreation(JavaUtils.javaBytePrimitiveType)(JavaUtils.javaArrayInitializer(LibLists.map(((w: number) => JavaUtils.javaLiteralToJavaExpression(({ tag: "integer", value: LibLiterals.int32ToBigint(w) }))))(byteValues)));
})())((_m as any).value);
    case "boolean": return ((b: boolean) => encodeLiteral_litExp(JavaUtils.javaBoolean(b)))((_m as any).value);
    case "float": return ((f: Core.FloatValue) => encodeLiteral_encodeFloat(f))((_m as any).value);
    case "integer": return ((i: Core.IntegerValue) => encodeLiteral_encodeInteger(i))((_m as any).value);
    case "string": return ((s: string) => encodeLiteral_litExp(JavaUtils.javaString(s)))((_m as any).value);
  }
})();
}

export function encodeLiteralType<t0, t1, t2>(lt: Core.LiteralType): ((x: t0) => ((x: t1) => t2 | JavaSyntax.Type)) {
  return ((cx: t0) => ((g: t1) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => ({ tag: "right", value: ({ tag: "reference", value: ({ tag: "array", value: ({
    dims: [[]],
    variant: ({ tag: "primitive", value: ({
    type: ({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "byte" }) }) }),
    annotations: []
  }) })
  }) }) }) }))((_m as any).value);
    case "boolean": return ((_: void) => encodeLiteralType_simple("Boolean")(cx)(g))((_m as any).value);
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => ({ tag: "right", value: JavaUtils.javaRefType([])(JavaNames.javaPackageName(["java", "math"]))("BigDecimal") }))((_m as any).value);
    case "float32": return ((_: void) => encodeLiteralType_simple("Float")(cx)(g))((_m as any).value);
    case "float64": return ((_: void) => encodeLiteralType_simple("Double")(cx)(g))((_m as any).value);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => ({ tag: "right", value: JavaUtils.javaRefType([])(JavaNames.javaPackageName(["java", "math"]))("BigInteger") }))((_m as any).value);
    case "int8": return ((_: void) => encodeLiteralType_simple("Byte")(cx)(g))((_m as any).value);
    case "int16": return ((_: void) => encodeLiteralType_simple("Short")(cx)(g))((_m as any).value);
    case "int32": return ((_: void) => encodeLiteralType_simple("Integer")(cx)(g))((_m as any).value);
    case "int64": return ((_: void) => encodeLiteralType_simple("Long")(cx)(g))((_m as any).value);
    case "uint8": return ((_: void) => encodeLiteralType_simple("Short")(cx)(g))((_m as any).value);
    case "uint16": return ((_: void) => encodeLiteralType_simple("Character")(cx)(g))((_m as any).value);
    case "uint32": return ((_: void) => encodeLiteralType_simple("Long")(cx)(g))((_m as any).value);
    case "uint64": return ((_: void) => ({ tag: "right", value: JavaUtils.javaRefType([])(JavaNames.javaPackageName(["java", "math"]))("BigInteger") }))((_m as any).value);
  }
})())((_m as any).value);
    case "string": return ((_: void) => encodeLiteralType_simple("String")(cx)(g))((_m as any).value);
  }
})()));
}

export function encodeLiteralType_simple<t0, t1, t2>(n: string): ((x: t0) => ((x: t1) => t2 | JavaSyntax.Type)) {
  return ((cx: t0) => ((g: t1) => ({ tag: "right", value: JavaUtils.javaRefType([])(null)(n) })));
}

export function encodeLiteral_encodeFloat(f: Core.FloatValue): JavaSyntax.Expression {
  return (() => {
  const _m = f;
  switch (_m.tag) {
    case "bigfloat": return ((v: number) => JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("java.math.BigDecimal")(null))([encodeLiteral(({ tag: "string", value: LibLiterals.showBigfloat(v) }))])(null))((_m as any).value);
    case "float32": return ((v: number) => encodeLiteral_encodeFloat32(v))((_m as any).value);
    case "float64": return ((v: number) => encodeLiteral_encodeFloat64(v))((_m as any).value);
  }
})();
}

export function encodeLiteral_encodeFloat32(v: number): JavaSyntax.Expression {
  return (() => {
  const s = LibLiterals.showFloat32(v);
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))(encodeLiteral_javaSpecialFloatExpr("Float")("NaN"))(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))(encodeLiteral_javaSpecialFloatExpr("Float")("POSITIVE_INFINITY"))(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))(encodeLiteral_javaSpecialFloatExpr("Float")("NEGATIVE_INFINITY"))(encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "floatingPoint", value: ({ tag: "float" }) }) }))(encodeLiteral_litExp(({ tag: "floatingPoint", value: LibLiterals.float32ToBigfloat(v) }))))));
})();
}

export function encodeLiteral_encodeFloat64(v: number): JavaSyntax.Expression {
  return (() => {
  const s = LibLiterals.showFloat64(v);
  return LibLogic.ifElse(LibEquality.equal(s)("NaN"))(encodeLiteral_javaSpecialFloatExpr("Double")("NaN"))(LibLogic.ifElse(LibEquality.equal(s)("Infinity"))(encodeLiteral_javaSpecialFloatExpr("Double")("POSITIVE_INFINITY"))(LibLogic.ifElse(LibEquality.equal(s)("-Infinity"))(encodeLiteral_javaSpecialFloatExpr("Double")("NEGATIVE_INFINITY"))(LibLogic.ifElse(LibEquality.equal(s)("-0.0"))(encodeLiteral_javaParseDouble("-0.0"))(encodeLiteral_litExp(({ tag: "floatingPoint", value: LibLiterals.float64ToBigfloat(v) }))))));
})();
}

export function encodeLiteral_encodeInteger(i: Core.IntegerValue): JavaSyntax.Expression {
  return (() => {
  const _m = i;
  switch (_m.tag) {
    case "bigint": return ((v: bigint) => JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("java.math.BigInteger")(null))([encodeLiteral(({ tag: "string", value: LibLiterals.showBigint(v) }))])(null))((_m as any).value);
    case "int8": return ((v: number) => encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "byte" }) }) }))(encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.int8ToBigint(v) }))))((_m as any).value);
    case "int16": return ((v: bigint) => encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "short" }) }) }))(encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.int16ToBigint(v) }))))((_m as any).value);
    case "int32": return ((v: number) => encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.int32ToBigint(v) })))((_m as any).value);
    case "int64": return ((v: bigint) => encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "long" }) }) }))(encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.int64ToBigint(v) }))))((_m as any).value);
    case "uint8": return ((v: bigint) => encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "short" }) }) }))(encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.uint8ToBigint(v) }))))((_m as any).value);
    case "uint16": return ((v: number) => encodeLiteral_litExp(({ tag: "character", value: v })))((_m as any).value);
    case "uint32": return ((v: bigint) => encodeLiteral_primCast(({ tag: "numeric", value: ({ tag: "integral", value: ({ tag: "long" }) }) }))(encodeLiteral_litExp(({ tag: "integer", value: LibLiterals.uint32ToBigint(v) }))))((_m as any).value);
    case "uint64": return ((v: bigint) => JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("java.math.BigInteger")(null))([encodeLiteral(({ tag: "string", value: LibLiterals.showBigint(LibLiterals.uint64ToBigint(v)) }))])(null))((_m as any).value);
  }
})();
}

export function encodeLiteral_javaParseDouble(value: string): JavaSyntax.Expression {
  return JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("Double")("parseDouble")([encodeLiteral(({ tag: "string", value: value }))]));
}

export function encodeLiteral_javaSpecialFloatExpr(className: string): ((x: string) => JavaSyntax.Expression) {
  return ((fieldName: string) => JavaUtils.javaExpressionNameToJavaExpression(({
    qualifier: [className],
    identifier: fieldName
  })));
}

export function encodeLiteral_litExp(l: JavaSyntax.Literal): JavaSyntax.Expression {
  return JavaUtils.javaLiteralToJavaExpression(l);
}

export function encodeLiteral_primCast(pt: JavaSyntax.PrimitiveType): ((x: JavaSyntax.Expression) => JavaSyntax.Expression) {
  return ((expr: JavaSyntax.Expression) => JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastPrimitive(pt)(JavaUtils.javaExpressionToJavaUnaryExpression(expr))));
}

export function encodeNullaryConstant<t0, t1, t2, t3, t4>(env: t0): ((x: t1) => ((x: Core.Term) => ((x: t2) => ((x: t3) => Errors.Error | t4)))) {
  return ((typ: t1) => ((funTerm: Core.Term) => ((cx: t2) => ((g: t3) => ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("unexpected ")(LibStrings.cat2("nullary function")(LibStrings.cat2(" in ")(ShowCore.term(funTerm)))) }) })))));
}

export function encodeNullaryConstant_typeArgsFromReturnType<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Type) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<JavaSyntax.TypeArgument>))) {
  return ((t: Core.Type) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "set": return ((st: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(st)(cx)(g))(((jst: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jst)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: [({ tag: "reference", value: rt })] }))))))((_m as any).value);
    case "list": return ((lt_: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(lt_)(cx)(g))(((jlt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jlt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: [({ tag: "reference", value: rt })] }))))))((_m as any).value);
    case "maybe": return ((mt: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(mt)(cx)(g))(((jmt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jmt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: [({ tag: "reference", value: rt })] }))))))((_m as any).value);
    case "map": return ((mp: Core.MapType) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(((_x) => _x.keys)(mp))(cx)(g))(((jkt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jkt)(cx))(((rk: JavaSyntax.ReferenceType) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(((_x) => _x.values)(mp))(cx)(g))(((jvt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jvt)(cx))(((rv: JavaSyntax.ReferenceType) => ({ tag: "right", value: [({ tag: "reference", value: rk }), ({ tag: "reference", value: rv })] }))))))))))((_m as any).value);
    default: return ({ tag: "right", value: [] })(_m);
  }
})())));
}

export function encodeNullaryPrimitiveByName<t0>(env: JavaEnvironment.JavaEnvironment): ((x: Core.Type) => ((x: Core.Name) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))) {
  return ((typ: Core.Type) => ((name: Core.Name) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return LibEithers.bind(encodeNullaryConstant_typeArgsFromReturnType(aliases)(typ)(cx)(g))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => LibLogic.ifElse(LibLists.null_(targs))((() => {
  const header = ({ tag: "simple", value: elementJavaIdentifier(true)(false)(aliases)(name) });
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(({
    header: header,
    arguments: []
  })) });
})())((() => {
  const fullName = ((_x) => _x)(elementJavaIdentifier(true)(false)(aliases)(name));
  return (() => {
  const parts = LibStrings.splitOn(".")(fullName);
  return (() => {
  const className = LibStrings.intercalate(".")(LibLists.init(parts));
  return (() => {
  const methodName = LibLists.last(parts);
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs(className)(methodName)(targs)([])) });
})();
})();
})();
})())));
})()))));
}

export function encodeTerm(env: JavaEnvironment.JavaEnvironment): ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))) {
  return ((term: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => encodeTermInternal(env)([])([])(term)(cx)(g))));
}

export function encodeTermDefinition(env: JavaEnvironment.JavaEnvironment): ((x: Packaging.TermDefinition) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.InterfaceMemberDeclaration))) {
  return ((tdef: Packaging.TermDefinition) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const name = ((_x) => _x.name)(tdef);
  return (() => {
  const term0 = ((_x) => _x.term)(tdef);
  return (() => {
  const ts = LibMaybes.maybe(({
    variables: [],
    type: ({ tag: "variable", value: "hydra.core.Unit" }),
    constraints: null
  }))(((x: Core.TypeScheme) => x))(((_x) => _x.type)(tdef));
  return (() => {
  const term = Variables.unshadowVariables(term0);
  return LibEithers.bind(analyzeJavaFunction(env)(term)(cx)(g))(((fs: Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>) => (() => {
  const schemeVars = LibLists.filter(((v: Core.Name) => isSimpleName(v)))(((_x) => _x.variables)(ts));
  return (() => {
  const termVars = ((_x) => _x.typeParams)(fs);
  return (() => {
  const schemeTypeVars = collectTypeVars(((_x) => _x.type)(ts));
  return (() => {
  const usedSchemeVars = LibLists.filter(((v: Core.Name) => LibSets.member(v)(schemeTypeVars)))(schemeVars);
  return (() => {
  const tparams = LibLogic.ifElse(LibLists.null_(usedSchemeVars))(termVars)(usedSchemeVars);
  return (() => {
  const params = ((_x) => _x.params)(fs);
  return (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const body = ((_x) => _x.body)(fs);
  return (() => {
  const doms = ((_x) => _x.domains)(fs);
  return (() => {
  const env2 = ((_x) => _x.environment)(fs);
  return (() => {
  const schemeType = ((_x) => _x.type)(ts);
  return (() => {
  const numParams = LibLists.length(params);
  return (() => {
  const peelResult = peelDomainsAndCod(numParams)(schemeType);
  return (() => {
  const schemeDoms = LibPairs.first(peelResult);
  return (() => {
  const cod = LibPairs.second(peelResult);
  return (() => {
  const schemeVarSet = LibSets.fromList(tparams);
  return LibEithers.bind(LibLogic.ifElse(LibLists.null_(tparams))(({ tag: "right", value: LibMaps.empty }))(buildSubstFromAnnotations(schemeVarSet)(term)(cx)(g)))(((typeVarSubst: ReadonlyMap<Core.Name, Core.Name>) => (() => {
  const overgenSubst = detectAccumulatorUnification(schemeDoms)(cod)(tparams);
  return (() => {
  const overgenVarSubst = LibMaps.fromList(LibMaybes.cat(LibLists.map(((entry: readonly [Core.Name, Core.Type]) => (() => {
  const k = LibPairs.first(entry);
  return (() => {
  const v = LibPairs.second(entry);
  return (() => {
  const _m = v;
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => [k, n])((_m as any).value);
    default: return null(_m);
  }
})();
})();
})()))(LibMaps.toList(overgenSubst))));
  return (() => {
  const fixedCod = LibLogic.ifElse(LibMaps.null_(overgenSubst))(cod)(substituteTypeVarsWithTypes(overgenSubst)(cod));
  return (() => {
  const fixedDoms = LibLogic.ifElse(LibMaps.null_(overgenSubst))(schemeDoms)(LibLists.map(((d: Core.Type) => substituteTypeVarsWithTypes(overgenSubst)(d)))(schemeDoms));
  return (() => {
  const fixedTparams = LibLogic.ifElse(LibMaps.null_(overgenSubst))(tparams)(LibLists.filter(((v: Core.Name) => LibLogic.not(LibMaps.member(v)(overgenSubst))))(tparams));
  return (() => {
  const constraints = LibMaybes.fromMaybe(LibMaps.empty)(((_x) => _x.constraints)(ts));
  return (() => {
  const jparams = LibLists.map(((v: Core.Name) => JavaUtils.javaTypeParameter(Formatting.capitalize(((_x) => _x)(v)))))(fixedTparams);
  return (() => {
  const aliases2base = ((_x) => _x.aliases)(env2);
  return (() => {
  const trustedVars = LibSets.unions(LibLists.map(((d: Core.Type) => collectTypeVars(d)))(LibLists.concat2(fixedDoms)([fixedCod])));
  return (() => {
  const fixedSchemeVarSet = LibSets.fromList(fixedTparams);
  return (() => {
  const aliases2 = ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases2base),
    packages: ((_x) => _x.packages)(aliases2base),
    branchVars: ((_x) => _x.branchVars)(aliases2base),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases2base),
    inScopeTypeParams: fixedSchemeVarSet,
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases2base),
    inScopeJavaVars: ((_x) => _x.inScopeJavaVars)(aliases2base),
    varRenames: ((_x) => _x.varRenames)(aliases2base),
    lambdaVars: LibSets.union(((_x) => _x.lambdaVars)(aliases2base))(LibSets.fromList(params)),
    typeVarSubst: LibMaps.union(overgenVarSubst)(typeVarSubst),
    trustedTypeVars: LibSets.intersection(trustedVars)(fixedSchemeVarSet),
    methodCodomain: fixedCod,
    thunkedVars: ((_x) => _x.thunkedVars)(aliases2base)
  });
  return (() => {
  const env2WithTypeParams = ({
    aliases: aliases2,
    graph: ((_x) => _x.graph)(env2)
  });
  return LibEithers.bind(bindingsToStatements(env2WithTypeParams)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env3 = LibPairs.second(bindResult);
  return LibEithers.bind(LibLogic.ifElse(LibMaps.null_(overgenSubst))(({ tag: "right", value: body }))(applyOvergenSubstToTermAnnotations(overgenSubst)(body)(cx)(g)))(((body_: Core.Term) => (() => {
  const annotatedBody = propagateTypesInAppChain(fixedCod)(fixedCod)(body_);
  return LibEithers.bind(LibEithers.mapList(((pair: readonly [Core.Type, Core.Name]) => LibEithers.bind(encodeType(aliases2)(LibSets.empty)(LibPairs.first(pair))(cx)(g))(((jdom: JavaSyntax.Type) => ({ tag: "right", value: JavaUtils.javaTypeToJavaFormalParameter(jdom)(LibPairs.second(pair)) })))))(LibLists.zip(fixedDoms)(params)))(((jformalParams: ReadonlyArray<JavaSyntax.FormalParameter>) => LibEithers.bind(encodeType(aliases2)(LibSets.empty)(fixedCod)(cx)(g))(((jcod: JavaSyntax.Type) => (() => {
  const result = JavaUtils.javaTypeToJavaResult(jcod);
  return (() => {
  const mods = [({ tag: "static" })];
  return (() => {
  const jname = JavaUtils.sanitizeJavaName(Formatting.decapitalize(Names.localNameOf(name)));
  return (() => {
  const isTCO = false;
  return LibEithers.bind(LibLogic.ifElse(isTCO)((() => {
  const tcoSuffix = "_tco";
  return (() => {
  const snapshotNames = LibLists.map(((p: Core.Name) => LibStrings.cat2(((_x) => _x)(p))(tcoSuffix)))(params);
  return (() => {
  const tcoVarRenames = LibMaps.fromList(LibLists.zip(params)(snapshotNames));
  return (() => {
  const snapshotDecls = LibLists.map(((pair: readonly [Core.Name, Core.Name]) => JavaUtils.finalVarDeclarationStatement(JavaUtils.variableToJavaIdentifier(LibPairs.second(pair)))(JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(LibPairs.first(pair))))))(LibLists.zip(params)(snapshotNames));
  return (() => {
  const tcoBody = LibLogic.ifElse(LibLists.null_(bindings))(annotatedBody)(({ tag: "let", value: ({
    bindings: bindings,
    body: annotatedBody
  }) }));
  return LibEithers.bind(encodeTermTCO(env2WithTypeParams)(name)(params)(tcoVarRenames)(0)(tcoBody)(cx)(g))(((tcoStmts: ReadonlyArray<JavaSyntax.BlockStatement>) => (() => {
  const whileBodyStmts = LibLists.concat2(snapshotDecls)(tcoStmts);
  return (() => {
  const whileBodyBlock = ({ tag: "withoutTrailing", value: ({ tag: "block", value: whileBodyStmts }) });
  return (() => {
  const noCond = null;
  return (() => {
  const whileStmt = ({ tag: "statement", value: ({ tag: "while", value: ({
    cond: noCond,
    body: whileBodyBlock
  }) }) });
  return ({ tag: "right", value: [whileStmt] });
})();
})();
})();
})()));
})();
})();
})();
})();
})())(LibEithers.bind(encodeTerm(env3)(annotatedBody)(cx)(g))(((jbody: JavaSyntax.Expression) => (() => {
  const returnSt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jbody) });
  return ({ tag: "right", value: LibLists.concat2(bindingStmts)([returnSt]) });
})()))))(((methodBody: ReadonlyArray<JavaSyntax.BlockStatement>) => ({ tag: "right", value: JavaUtils.interfaceMethodDeclaration(mods)(jparams)(jname)(jformalParams)(result)(methodBody) })));
})();
})();
})();
})()))));
})()));
})();
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

export function encodeTermInternal(env: JavaEnvironment.JavaEnvironment): ((x: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((x: ReadonlyArray<JavaSyntax.Type>) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))))) {
  return ((anns: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((tyapps: ReadonlyArray<JavaSyntax.Type>) => ((term: Core.Term) => ((cx: Context.Context) => ((g0: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  const g = ((_x) => _x.graph)(env);
  const encode = ((t: Core.Term) => encodeTerm(env)(t)(cx)(g));
  return (() => {
  const _m = term;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => encodeTermInternal(env)(LibLists.cons(((_x) => _x.annotation)(at))(anns))(tyapps)(((_x) => _x.body)(at))(cx)(g))((_m as any).value);
    case "application": return ((app: Core.Application) => encodeApplication(env)(app)(cx)(g))((_m as any).value);
    case "either": return ((et: Core.Term | Core.Term) => LibEithers.bind(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: null }))(LibEithers.bind(takeTypeArgs("either")(2)(tyapps)(cx)(g))(((ta: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: ta })))))(((mtargs: ReadonlyArray<JavaSyntax.TypeArgument> | null) => (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mEitherType: Core.Type | null) => (() => {
  const branchTypes = LibMaybes.bind(mEitherType)(((etyp: Core.Type) => (() => {
  const _m = Strip.deannotateType(etyp);
  switch (_m.tag) {
    case "either": return ((et2: Core.EitherType) => [((_x) => _x.left)(et2), ((_x) => _x.right)(et2)])((_m as any).value);
    default: return null(_m);
  }
})()));
  return (() => {
  const encodeWithType = ((branchType: Core.Type) => ((t1: Core.Term) => (() => {
  const annotated = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(branchType))(t1);
  return encodeTermInternal(env)(anns)([])(annotated)(cx)(g);
})()));
  return (() => {
  const eitherCall = ((methodName: string) => ((expr: JavaSyntax.Expression) => LibMaybes.cases(mtargs)(JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("hydra.util.Either")(methodName)([expr])))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("hydra.util.Either")(methodName)(targs)([expr]))))));
  return LibEithers.either(((term1: Core.Term) => LibEithers.bind(LibMaybes.cases(branchTypes)(encode(term1))(((bt: readonly [Core.Type, Core.Type]) => encodeWithType(LibPairs.first(bt))(term1))))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: eitherCall("left")(expr) })))))(((term1: Core.Term) => LibEithers.bind(LibMaybes.cases(branchTypes)(encode(term1))(((bt: readonly [Core.Type, Core.Type]) => encodeWithType(LibPairs.second(bt))(term1))))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: eitherCall("right")(expr) })))))(et);
})();
})();
})()));
})())))((_m as any).value);
    case "lambda": return ((_lam: Core.Lambda) => encodeFunctionFormTerm(env)(anns)(term)(cx)(g))((_m as any).value);
    case "project": return ((_p: Core.Projection) => encodeFunctionFormTerm(env)(anns)(term)(cx)(g))((_m as any).value);
    case "cases": return ((_c: Core.CaseStatement) => encodeFunctionFormTerm(env)(anns)(term)(cx)(g))((_m as any).value);
    case "unwrap": return ((_w: Core.Name) => encodeFunctionFormTerm(env)(anns)(term)(cx)(g))((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const bindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const body = ((_x) => _x.body)(lt);
  return LibLogic.ifElse(LibLists.null_(bindings))(encodeTermInternal(env)(anns)([])(body)(cx)(g))(LibEithers.bind(bindingsToStatements(env)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env2 = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTermInternal(env2)(anns)([])(body)(cx)(g))(((jbody: JavaSyntax.Expression) => (() => {
  const returnSt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jbody) });
  return (() => {
  const block = LibLists.concat2(bindingStmts)([returnSt]);
  return (() => {
  const nullaryLambda = ({ tag: "lambda", value: ({
    parameters: ({ tag: "tuple", value: [] }),
    body: ({ tag: "block", value: block })
  }) });
  return (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return (() => {
  const g2 = ((_x) => _x.graph)(env2);
  return (() => {
  const aliases2 = ((_x) => _x.aliases)(env2);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mt: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mt)(Checking.typeOfTerm(cx)(g2)(body))(((t: Core.Type) => ({ tag: "right", value: t }))))(((letType: Core.Type) => LibEithers.bind(encodeType(aliases2)(LibSets.empty)(letType)(cx)(g))(((jLetType: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jLetType)(cx))(((rt: JavaSyntax.ReferenceType) => (() => {
  const supplierRt = ({ tag: "classOrInterface", value: ({ tag: "class", value: JavaUtils.javaClassType([rt])(JavaNames.javaUtilFunctionPackageName)("Supplier") }) });
  return (() => {
  const castExpr = JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(supplierRt)(JavaUtils.javaExpressionToJavaUnaryExpression(nullaryLambda)));
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(({ tag: "right", value: JavaUtils.javaExpressionToJavaPrimary(castExpr) }))("get")([])) });
})();
})()))))))));
})();
})();
})();
})();
})();
})()));
})();
})())));
})();
})())((_m as any).value);
    case "list": return ((els: ReadonlyArray<Core.Term>) => LibLogic.ifElse(LibLists.null_(els))(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Collections")("emptyList")([])) }))(LibEithers.bind(takeTypeArgs("list")(1)(tyapps)(cx)(g))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptyList")(targs)([])) })))))(LibEithers.bind(LibEithers.mapList(encode)(els))(((jels: ReadonlyArray<JavaSyntax.Expression>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Arrays")("asList")(jels)) })))))((_m as any).value);
    case "literal": return ((l: Core.Literal) => ({ tag: "right", value: encodeLiteral(l) }))((_m as any).value);
    case "map": return ((m: ReadonlyMap<Core.Term, Core.Term>) => LibLogic.ifElse(LibMaps.null_(m))(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Collections")("emptyMap")([])) }))(LibEithers.bind(takeTypeArgs("map")(2)(tyapps)(cx)(g))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptyMap")(targs)([])) })))))(LibEithers.bind(LibEithers.mapList(encode)(LibMaps.keys(m)))(((jkeys: ReadonlyArray<JavaSyntax.Expression>) => LibEithers.bind(LibEithers.mapList(encode)(LibMaps.elems(m)))(((jvals: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const pairExprs = LibLists.map(((kv: readonly [JavaSyntax.Expression, JavaSyntax.Expression]) => JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Map")("entry")([LibPairs.first(kv), LibPairs.second(kv)]))))(LibLists.zip(jkeys)(jvals));
  return (() => {
  const innerMap = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Map")("ofEntries")(pairExprs));
  return ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("java.util.TreeMap")(null))([innerMap])(null) });
})();
})()))))))((_m as any).value);
    case "maybe": return ((mt: Core.Term | null) => LibMaybes.cases(mt)(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("hydra.util.Maybe")("nothing")([])) }))(LibEithers.bind(takeTypeArgs("maybe")(1)(tyapps)(cx)(g))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("hydra.util.Maybe")("nothing")(targs)([])) })))))(((term1: Core.Term) => LibEithers.bind(encode(term1))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("hydra.util.Maybe")("just")([expr])) }))))))((_m as any).value);
    case "pair": return ((p: readonly [Core.Term, Core.Term]) => LibEithers.bind(encode(LibPairs.first(p)))(((jterm1: JavaSyntax.Expression) => LibEithers.bind(encode(LibPairs.second(p)))(((jterm2: JavaSyntax.Expression) => LibEithers.bind(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: null }))(LibEithers.bind(LibEithers.mapList(((jt: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt)(cx)))(tyapps))(((rts: ReadonlyArray<JavaSyntax.ReferenceType>) => ({ tag: "right", value: ({ tag: "arguments", value: LibLists.map(((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt })))(rts) }) })))))(((mtargs: JavaSyntax.TypeArgumentsOrDiamond | null) => ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("hydra.util.Pair")(mtargs))([jterm1, jterm2])(null) }))))))))((_m as any).value);
    case "record": return ((rec: Core.Record) => (() => {
  const recName = ((_x) => _x.typeName)(rec);
  return (() => {
  const mRecordType = LibEithers.either(((_: Errors.Error) => null))(((t: Core.Type) => t))(Resolution.requireType(cx)(g)(recName));
  return (() => {
  const strippedRecTyp = LibMaybes.map(((recTyp: Core.Type) => stripForalls(Strip.deannotateType(recTyp))))(mRecordType);
  return (() => {
  const mFieldTypeMap = LibMaybes.bind(strippedRecTyp)(((bodyTyp: Core.Type) => (() => {
  const _m = bodyTyp;
  switch (_m.tag) {
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibMaps.fromList(LibLists.map(((ft: Core.FieldType) => [((_x) => _x.name)(ft), ((_x) => _x.type)(ft)]))(rt)))((_m as any).value);
    default: return null(_m);
  }
})()));
  return (() => {
  const combinedAnnsRec = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnnsRec)))(((mAnnotType: Core.Type | null) => (() => {
  const mTypeSubst = LibMaybes.bind(mAnnotType)(((annTyp: Core.Type) => LibMaybes.bind(mRecordType)(((recTyp: Core.Type) => (() => {
  const args = extractTypeApplicationArgs(Strip.deannotateType(annTyp));
  return (() => {
  const params = collectForallParams(Strip.deannotateType(recTyp));
  return LibLogic.ifElse(LibLogic.or(LibLists.null_(args))(LibLogic.not(LibEquality.equal(LibLists.length(args))(LibLists.length(params)))))(null)(LibMaps.fromList(LibLists.zip(params)(args)));
})();
})()))));
  return (() => {
  const encodeField = ((fld: Core.Field) => LibMaybes.cases(mFieldTypeMap)(encode(((_x) => _x.term)(fld)))(((ftmap: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const mftyp = LibMaps.lookup(((_x) => _x.name)(fld))(ftmap);
  return LibMaybes.cases(mftyp)(encode(((_x) => _x.term)(fld)))(((ftyp: Core.Type) => (() => {
  const resolvedType = LibMaybes.cases(mTypeSubst)(ftyp)(((subst: ReadonlyMap<Core.Name, Core.Type>) => applySubstFull(subst)(ftyp)));
  return (() => {
  const annotatedFieldTerm = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(resolvedType))(((_x) => _x.term)(fld));
  return encodeTermInternal(env)(anns)([])(annotatedFieldTerm)(cx)(g);
})();
})()));
})())));
  return LibEithers.bind(LibEithers.mapList(encodeField)(((_x) => _x.fields)(rec)))(((fieldExprs: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const consId = JavaUtils.nameToJavaName(aliases)(recName);
  return LibEithers.bind(LibLogic.ifElse(LibLogic.not(LibLists.null_(tyapps)))(LibEithers.bind(LibEithers.mapList(((jt: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt)(cx)))(tyapps))(((rts: ReadonlyArray<JavaSyntax.ReferenceType>) => ({ tag: "right", value: ({ tag: "arguments", value: LibLists.map(((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt })))(rts) }) }))))((() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mtyp: Core.Type | null) => LibMaybes.cases(mtyp)(({ tag: "right", value: null }))(((annTyp: Core.Type) => (() => {
  const typeArgs = extractTypeApplicationArgs(Strip.deannotateType(annTyp));
  return LibLogic.ifElse(LibLists.null_(typeArgs))(({ tag: "right", value: null }))(LibEithers.bind(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt)(cx)))))(typeArgs))(((jTypeArgs: ReadonlyArray<JavaSyntax.ReferenceType>) => ({ tag: "right", value: ({ tag: "arguments", value: LibLists.map(((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt })))(jTypeArgs) }) }))));
})()))));
})()))(((mtargs: JavaSyntax.TypeArgumentsOrDiamond | null) => ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(consId)(mtargs))(fieldExprs)(null) })));
})()));
})();
})()));
})();
})();
})();
})();
})())((_m as any).value);
    case "set": return ((s: ReadonlySet<Core.Term>) => LibLogic.ifElse(LibSets.null_(s))(LibLogic.ifElse(LibLists.null_(tyapps))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Collections")("emptySet")([])) }))(LibEithers.bind(takeTypeArgs("set")(1)(tyapps)(cx)(g))(((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("java.util.Collections")("emptySet")(targs)([])) })))))((() => {
  const slist = LibSets.toList(s);
  return LibEithers.bind(LibEithers.mapList(encode)(slist))(((jels: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const innerSet = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStatic("java.util.Set")("of")(jels));
  return ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("java.util.TreeSet")(null))([innerSet])(null) });
})()));
})()))((_m as any).value);
    case "typeLambda": return ((tl: Core.TypeLambda) => withTypeLambda(env)(tl)(((env2: JavaEnvironment.JavaEnvironment) => (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mtyp: Core.Type | null) => (() => {
  const annotatedBody = LibMaybes.cases(mtyp)(((_x) => _x.body)(tl))(((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "forall": return ((fa: Core.ForallType) => Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(((_x) => _x.body)(fa)))(((_x) => _x.body)(tl)))((_m as any).value);
    default: return ((_x) => _x.body)(tl)(_m);
  }
})()));
  return encodeTerm(env2)(annotatedBody)(cx)(g);
})()));
})())))((_m as any).value);
    case "inject": return ((inj: Core.Injection) => (() => {
  const injTypeName = ((_x) => _x.typeName)(inj);
  return (() => {
  const injField = ((_x) => _x.field)(inj);
  return (() => {
  const injFieldName = ((_x) => _x.name)(injField);
  return (() => {
  const injFieldTerm = ((_x) => _x.term)(injField);
  return (() => {
  const typeId = ((_x) => _x)(JavaUtils.nameToJavaName(aliases)(injTypeName));
  return (() => {
  const consId = LibStrings.cat([typeId, ".", JavaUtils.sanitizeJavaName(Formatting.capitalize(((_x) => _x)(injFieldName)))]);
  return LibEithers.bind(isFieldUnitType(injTypeName)(injFieldName)(cx)(g))(((fieldIsUnit: boolean) => LibEithers.bind(LibLogic.ifElse(LibLogic.or(Predicates.isUnitTerm(Strip.deannotateTerm(injFieldTerm)))(fieldIsUnit))(({ tag: "right", value: [] }))(LibEithers.bind(encode(injFieldTerm))(((ex: JavaSyntax.Expression) => ({ tag: "right", value: [ex] })))))(((args: ReadonlyArray<JavaSyntax.Expression>) => ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(consId)(null))(args)(null) })))));
})();
})();
})();
})();
})();
})())((_m as any).value);
    case "variable": return ((name: Core.Name) => LibMaybes.cases(LibMaps.lookup(name)(((_x) => _x.primitives)(g)))(encodeVariable(env)(name)(cx)(g))(((_prim: Graph.Primitive) => (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mt: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mt)(Checking.typeOfTerm(cx)(g)(term))(((t: Core.Type) => ({ tag: "right", value: t }))))(((typ: Core.Type) => (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => encodeFunctionPrimitiveByName(env)(((_x) => _x.domain)(ft))(((_x) => _x.codomain)(ft))(name)(cx)(g))((_m as any).value);
    default: return encodeNullaryPrimitiveByName(env)(typ)(name)(cx)(g)(_m);
  }
})()))));
})())))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: JavaUtils.javaLiteralToJavaExpression(({ tag: "null" })) }))((_m as any).value);
    case "wrap": return ((wt: Core.WrappedTerm) => LibEithers.bind(encode(((_x) => _x.body)(wt)))(((jarg: JavaSyntax.Expression) => ({ tag: "right", value: JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(JavaUtils.nameToJavaName(aliases)(((_x) => _x.typeName)(wt)))(null))([jarg])(null) }))))((_m as any).value);
    case "typeApplication": return ((ta: Core.TypeApplicationTerm) => (() => {
  const atyp = ((_x) => _x.type)(ta);
  return (() => {
  const body = ((_x) => _x.body)(ta);
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(atyp)(cx)(g))(((jatyp: JavaSyntax.Type) => (() => {
  const combinedAnns = LibLists.foldl(((acc: ReadonlyMap<Core.Name, Core.Term>) => ((m: ReadonlyMap<Core.Name, Core.Term>) => LibMaps.union(acc)(m))))(LibMaps.empty)(anns);
  return LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(combinedAnns)))(((mtyp: Core.Type | null) => LibEithers.bind(LibMaybes.cases(mtyp)(Checking.typeOfTerm(cx)(g)(term))(((t: Core.Type) => ({ tag: "right", value: t }))))(((typ: Core.Type) => (() => {
  const collected0 = collectTypeApps0(body)([atyp]);
  return (() => {
  const innermostBody0 = LibPairs.first(collected0);
  return (() => {
  const allTypeArgs0 = LibPairs.second(collected0);
  return LibEithers.bind(correctCastType(innermostBody0)(allTypeArgs0)(typ)(cx)(g))(((correctedTyp: Core.Type) => (() => {
  const collected = collectTypeApps(body)([atyp]);
  return (() => {
  const innermostBody = LibPairs.first(collected);
  return (() => {
  const allTypeArgs = LibPairs.second(collected);
  return (() => {
  const _m = innermostBody;
  switch (_m.tag) {
    case "variable": return ((varName: Core.Name) => LibEithers.bind(classifyDataReference(varName)(cx)(g))(((cls: JavaEnvironment.JavaSymbolClass) => typeAppNullaryOrHoisted(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(varName)(cls)(allTypeArgs)(cx)(g))))((_m as any).value);
    case "either": return ((eitherTerm: Core.Term | Core.Term) => LibLogic.ifElse(LibEquality.equal(LibLists.length(allTypeArgs))(2))((() => {
  const eitherBranchTypes = [LibLists.head(allTypeArgs), LibLists.head(LibLists.tail(allTypeArgs))];
  return LibEithers.bind(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt)(cx)))))(allTypeArgs))(((jTypeArgs: ReadonlyArray<JavaSyntax.ReferenceType>) => (() => {
  const eitherTargs = LibLists.map(((rt: JavaSyntax.ReferenceType) => ({ tag: "reference", value: rt })))(jTypeArgs);
  return (() => {
  const encodeEitherBranch = ((branchType: Core.Type) => ((t1: Core.Term) => (() => {
  const annotated = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(branchType))(t1);
  return encodeTermInternal(env)(anns)([])(annotated)(cx)(g);
})()));
  return LibEithers.either(((term1: Core.Term) => LibEithers.bind(encodeEitherBranch(LibPairs.first(eitherBranchTypes))(term1))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("left")(eitherTargs)([expr])) })))))(((term1: Core.Term) => LibEithers.bind(encodeEitherBranch(LibPairs.second(eitherBranchTypes))(term1))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs("hydra.util.Either")("right")(eitherTargs)([expr])) })))))(eitherTerm);
})();
})()));
})())(typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)))((_m as any).value);
    default: return typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)(_m);
  }
})();
})();
})();
})()));
})();
})();
})()))));
})()));
})();
})())((_m as any).value);
    default: return ({ tag: "right", value: encodeLiteral(({ tag: "string", value: "Unimplemented term variant" })) })(_m);
  }
})();
})())))));
}

export function encodeTermTCO(env0: JavaEnvironment.JavaEnvironment): ((x: Core.Name) => ((x: ReadonlyArray<Core.Name>) => ((x: ReadonlyMap<Core.Name, Core.Name>) => ((x: number) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<JavaSyntax.BlockStatement>))))))) {
  return ((funcName: Core.Name) => ((paramNames: ReadonlyArray<Core.Name>) => ((tcoVarRenames: ReadonlyMap<Core.Name, Core.Name>) => ((tcoDepth: number) => ((term: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const aliases0 = ((_x) => _x.aliases)(env0);
  return (() => {
  const env = ({
    aliases: ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases0),
    packages: ((_x) => _x.packages)(aliases0),
    branchVars: ((_x) => _x.branchVars)(aliases0),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases0),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases0),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases0),
    inScopeJavaVars: ((_x) => _x.inScopeJavaVars)(aliases0),
    varRenames: LibMaps.union(tcoVarRenames)(((_x) => _x.varRenames)(aliases0)),
    lambdaVars: ((_x) => _x.lambdaVars)(aliases0),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases0),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases0),
    methodCodomain: ((_x) => _x.methodCodomain)(aliases0),
    thunkedVars: ((_x) => _x.thunkedVars)(aliases0)
  }),
    graph: ((_x) => _x.graph)(env0)
  });
  return (() => {
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
  return LibLogic.ifElse(LibLogic.and(isSelfCall)(LibEquality.equal(LibLists.length(gatherArgs))(LibLists.length(paramNames))))((() => {
  const changePairs = LibLists.filter(((pair: readonly [Core.Name, Core.Term]) => LibLogic.not((() => {
  const _m = Strip.deannotateAndDetypeTerm(LibPairs.second(pair));
  switch (_m.tag) {
    case "variable": return ((n: Core.Name) => LibEquality.equal(n)(LibPairs.first(pair)))((_m as any).value);
    default: return false(_m);
  }
})())))(LibLists.zip(paramNames)(gatherArgs));
  return (() => {
  const changedParams = LibLists.map(LibPairs.first)(changePairs);
  return LibEithers.bind(LibEithers.mapList(((pair: readonly [Core.Name, Core.Term]) => encodeTerm(env)(LibPairs.second(pair))(cx)(g)))(changePairs))(((jChangedArgs: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const assignments = LibLists.map(((pair: readonly [Core.Name, JavaSyntax.Expression]) => (() => {
  const paramName = LibPairs.first(pair);
  return (() => {
  const jArg = LibPairs.second(pair);
  return ({ tag: "statement", value: JavaUtils.javaAssignmentStatement(({ tag: "expressionName", value: JavaUtils.javaIdentifierToJavaExpressionName(JavaUtils.variableToJavaIdentifier(paramName)) }))(jArg) });
})();
})()))(LibLists.zip(changedParams)(jChangedArgs));
  return (() => {
  const continueStmt = ({ tag: "statement", value: ({ tag: "withoutTrailing", value: ({ tag: "continue", value: null }) }) });
  return ({ tag: "right", value: LibLists.concat2(assignments)([continueStmt]) });
})();
})()));
})();
})())((() => {
  const _m = stripped;
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => (() => {
  const letBindings = ((_x) => _x.bindings)(lt);
  return (() => {
  const letBody = ((_x) => _x.body)(lt);
  return LibEithers.bind(bindingsToStatements(env)(letBindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const letStmts = LibPairs.first(bindResult);
  return (() => {
  const envAfterLet = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTermTCO(envAfterLet)(funcName)(paramNames)(tcoVarRenames)(tcoDepth)(letBody)(cx)(g))(((tcoBodyStmts: ReadonlyArray<JavaSyntax.BlockStatement>) => ({ tag: "right", value: LibLists.concat2(letStmts)(tcoBodyStmts) })));
})();
})()));
})();
})())((_m as any).value);
    default: return (() => {
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
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const tname = ((_x) => _x.typeName)(cs);
  return (() => {
  const dflt = ((_x) => _x.default)(cs);
  return (() => {
  const cases_ = ((_x) => _x.cases)(cs);
  return LibEithers.bind(domTypeArgs(aliases)(Resolution.nominalApplication(tname)([]))(cx)(g))(((domArgs: ReadonlyArray<JavaSyntax.TypeArgument>) => LibEithers.bind(encodeTerm(env)(arg)(cx)(g))(((jArgRaw: JavaSyntax.Expression) => (() => {
  const depthSuffix = LibLogic.ifElse(LibEquality.equal(tcoDepth)(0))("")(LibLiterals.showInt32(tcoDepth));
  return (() => {
  const matchVarId = JavaUtils.javaIdentifier(LibStrings.cat(["_tco_match_", Formatting.decapitalize(Names.localNameOf(tname)), depthSuffix]));
  return (() => {
  const matchDecl = JavaUtils.varDeclarationStatement(matchVarId)(jArgRaw);
  return (() => {
  const jArg = JavaUtils.javaIdentifierToJavaExpression(matchVarId);
  return LibEithers.bind(LibEithers.mapList(((field: Core.Field) => (() => {
  const fieldName = ((_x) => _x.name)(field);
  return (() => {
  const variantRefType = JavaUtils.nameToJavaReferenceType(aliases)(true)(domArgs)(tname)(Formatting.capitalize(((_x) => _x)(fieldName)));
  return (() => {
  const _m = Strip.deannotateTerm(((_x) => _x.term)(field));
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => withLambda(env)(lam)(((env2: JavaEnvironment.JavaEnvironment) => (() => {
  const lambdaParam = ((_x) => _x.parameter)(lam);
  return (() => {
  const branchBody = ((_x) => _x.body)(lam);
  return (() => {
  const env3 = insertBranchVar(lambdaParam)(env2);
  return (() => {
  const varId = JavaUtils.variableToJavaIdentifier(lambdaParam);
  return (() => {
  const castExpr = JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(variantRefType)(JavaUtils.javaExpressionToJavaUnaryExpression(jArg)));
  return (() => {
  const localDecl = JavaUtils.varDeclarationStatement(varId)(castExpr);
  return (() => {
  const isBranchTailCall = Analysis.isTailRecursiveInTailPosition(funcName)(branchBody);
  return LibEithers.bind(LibLogic.ifElse(isBranchTailCall)(encodeTermTCO(env3)(funcName)(paramNames)(tcoVarRenames)(LibMath.add(tcoDepth)(1))(branchBody)(cx)(g))(LibEithers.bind(analyzeJavaFunction(env3)(branchBody)(cx)(g))(((fs: Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>) => (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const innerBody = ((_x) => _x.body)(fs);
  return (() => {
  const env4 = ((_x) => _x.environment)(fs);
  return LibEithers.bind(bindingsToStatements(env4)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env5 = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTerm(env5)(innerBody)(cx)(g))(((jret: JavaSyntax.Expression) => (() => {
  const returnStmt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jret) });
  return ({ tag: "right", value: LibLists.concat2(bindingStmts)([returnStmt]) });
})()));
})();
})()));
})();
})();
})()))))(((bodyStmts: ReadonlyArray<JavaSyntax.BlockStatement>) => (() => {
  const relExpr = JavaUtils.javaInstanceOf(JavaUtils.javaUnaryExpressionToJavaRelationalExpression(JavaUtils.javaExpressionToJavaUnaryExpression(jArg)))(variantRefType);
  return (() => {
  const condExpr = JavaUtils.javaRelationalExpressionToJavaExpression(relExpr);
  return (() => {
  const blockStmts = LibLists.cons(localDecl)(bodyStmts);
  return (() => {
  const ifBody = ({ tag: "withoutTrailing", value: ({ tag: "block", value: blockStmts }) });
  return ({ tag: "right", value: ({ tag: "statement", value: ({ tag: "ifThen", value: ({
    expression: condExpr,
    statement: ifBody
  }) }) }) });
})();
})();
})();
})()));
})();
})();
})();
})();
})();
})();
})())))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: "TCO: case branch is not a lambda" }) })(_m);
  }
})();
})();
})()))(cases_))(((ifBlocks: ReadonlyArray<JavaSyntax.BlockStatement>) => LibEithers.bind(LibMaybes.cases(dflt)(({ tag: "right", value: [({ tag: "statement", value: JavaUtils.javaReturnStatement(jArg) })] }))(((d: Core.Term) => LibEithers.bind(encodeTerm(env)(d)(cx)(g))(((dExpr: JavaSyntax.Expression) => ({ tag: "right", value: [({ tag: "statement", value: JavaUtils.javaReturnStatement(dExpr) })] }))))))(((defaultStmt: ReadonlyArray<JavaSyntax.BlockStatement>) => ({ tag: "right", value: LibLists.concat([[matchDecl], ifBlocks, defaultStmt]) })))));
})();
})();
})();
})()))));
})();
})();
})();
})())((_m as any).value);
    default: return LibEithers.bind(encodeTerm(env)(term)(cx)(g))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: [({ tag: "statement", value: JavaUtils.javaReturnStatement(expr) })] })))(_m);
  }
})();
})())(LibEithers.bind(encodeTerm(env)(term)(cx)(g))(((expr: JavaSyntax.Expression) => ({ tag: "right", value: [({ tag: "statement", value: JavaUtils.javaReturnStatement(expr) })] }))));
})();
})();
})()(_m);
  }
})());
})();
})();
})();
})();
})();
})();
})();
})())))))));
}

export function encodeType<t0>(aliases: JavaEnvironment.Aliases): ((x: ReadonlySet<Core.Name>) => ((x: Core.Type) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Type)))) {
  return ((boundVars: ReadonlySet<Core.Name>) => ((t: Core.Type) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const inScopeTypeParams = ((_x) => _x.inScopeTypeParams)(aliases);
  return (() => {
  const typeVarSubst = ((_x) => _x.typeVarSubst)(aliases);
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "application": return ((at: Core.ApplicationType) => LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.function)(at))(cx)(g))(((jlhs: JavaSyntax.Type) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.argument)(at))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jrhs: JavaSyntax.ReferenceType) => JavaUtils.addJavaTypeParameter(jrhs)(jlhs)(cx))))))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.domain)(ft))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jdom: JavaSyntax.ReferenceType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.codomain)(ft))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jcod: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jdom, jcod])(JavaNames.javaUtilFunctionPackageName)("Function") }))))))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => LibEithers.bind(encodeType(aliases)(LibSets.insert(((_x) => _x.parameter)(fa))(boundVars))(((_x) => _x.body)(fa))(cx)(g))(((jbody: JavaSyntax.Type) => JavaUtils.addJavaTypeParameter(JavaUtils.javaTypeVariable(((_x) => _x)(((_x) => _x.parameter)(fa))))(jbody)(cx))))((_m as any).value);
    case "list": return ((et: Core.Type) => LibEithers.bind(encodeType(aliases)(boundVars)(et)(cx)(g))(((jet: JavaSyntax.Type) => LibEithers.bind(LibEithers.bind(({ tag: "right", value: jet }))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([rt])(JavaNames.javaUtilPackageName)("List") }))))))((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => encodeLiteralType(lt)(cx)(g))((_m as any).value);
    case "either": return ((et: Core.EitherType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.left)(et))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jlt: JavaSyntax.ReferenceType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.right)(et))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jrt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jlt, jrt])(JavaNames.hydraUtilPackageName)("Either") }))))))((_m as any).value);
    case "map": return ((mt: Core.MapType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.keys)(mt))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jkt: JavaSyntax.ReferenceType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.values)(mt))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jvt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jkt, jvt])(JavaNames.javaUtilPackageName)("Map") }))))))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.first)(pt))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jfirst: JavaSyntax.ReferenceType) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(((_x) => _x.second)(pt))(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jsecond: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jfirst, jsecond])(JavaNames.hydraUtilPackageName)("Pair") }))))))((_m as any).value);
    case "unit": return ((_: void) => ({ tag: "right", value: JavaUtils.javaRefType([])(JavaNames.javaLangPackageName)("Void") }))((_m as any).value);
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => LibLogic.ifElse(LibLists.null_(rt))(({ tag: "right", value: JavaUtils.javaRefType([])(JavaNames.javaLangPackageName)("Void") }))(({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous record type" }) })))((_m as any).value);
    case "maybe": return ((ot: Core.Type) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(ot)(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jot: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jot])(JavaNames.hydraUtilPackageName)("Maybe") }))))((_m as any).value);
    case "set": return ((st: Core.Type) => LibEithers.bind(LibEithers.bind(encodeType(aliases)(boundVars)(st)(cx)(g))(((jt_: JavaSyntax.Type) => JavaUtils.javaTypeToJavaReferenceType(jt_)(cx))))(((jst: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaRefType([jst])(JavaNames.javaUtilPackageName)("Set") }))))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous union type" }) }))((_m as any).value);
    case "variable": return ((name0: Core.Name) => (() => {
  const name = LibMaybes.fromMaybe(name0)(LibMaps.lookup(name0)(typeVarSubst));
  return LibEithers.bind(encodeType_resolveIfTypedef(aliases)(boundVars)(inScopeTypeParams)(name)(cx)(g))(((resolved: Core.Type | null) => LibMaybes.cases(resolved)(({ tag: "right", value: LibLogic.ifElse(LibLogic.or(LibSets.member(name)(boundVars))(LibSets.member(name)(inScopeTypeParams)))(({ tag: "reference", value: JavaUtils.javaTypeVariable(((_x) => _x)(name)) }))(LibLogic.ifElse(isLambdaBoundVariable(name))(({ tag: "reference", value: JavaUtils.javaTypeVariable(((_x) => _x)(name)) }))(LibLogic.ifElse(isUnresolvedInferenceVar(name))(({ tag: "reference", value: ({ tag: "classOrInterface", value: ({ tag: "class", value: JavaUtils.javaClassType([])(JavaNames.javaLangPackageName)("Object") }) }) }))(({ tag: "reference", value: JavaUtils.nameToJavaReferenceType(aliases)(true)([])(name)(null) })))) }))(((resolvedType: Core.Type) => encodeType(aliases)(boundVars)(resolvedType)(cx)(g)))));
})())((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "left", value: ({ tag: "other", value: "unexpected anonymous wrap type" }) }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("can't encode unsupported type in Java: ")(ShowCore.type(t)) }) })(_m);
  }
})();
})();
})()))));
}

export function encodeTypeDefinition(pkg: JavaSyntax.PackageDeclaration): ((x: JavaEnvironment.Aliases) => ((x: Packaging.TypeDefinition) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | readonly [Core.Name, JavaSyntax.CompilationUnit])))) {
  return ((aliases: JavaEnvironment.Aliases) => ((tdef: Packaging.TypeDefinition) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const name = ((_x) => _x.name)(tdef);
  return (() => {
  const typ = ((_x) => _x.type)(((_x) => _x.type)(tdef));
  return (() => {
  const serializable = isSerializableJavaType(typ);
  return (() => {
  const imports = LibLogic.ifElse(serializable)([({ tag: "singleType", value: JavaUtils.javaTypeName("java.io.Serializable") })])([]);
  return LibEithers.bind(toClassDecl(false)(serializable)(aliases)([])(name)(typ)(cx)(g))(((decl: JavaSyntax.ClassDeclaration) => LibEithers.bind(Annotations.getTypeDescription(cx)(g)(typ))(((comment: string | null) => (() => {
  const tdecl = ({
    value: ({ tag: "class", value: decl }),
    comments: comment
  });
  return ({ tag: "right", value: [name, ({ tag: "ordinary", value: ({
    package: pkg,
    imports: imports,
    types: [tdecl]
  }) })] });
})()))));
})();
})();
})();
})()))));
}

export function encodeType_resolveIfTypedef<t0, t1, t2>(aliases: t0): ((x: ReadonlySet<Core.Name>) => ((x: ReadonlySet<Core.Name>) => ((x: Core.Name) => ((x: t1) => ((x: Graph.Graph) => t2 | Core.Type | null))))) {
  return ((boundVars: ReadonlySet<Core.Name>) => ((inScopeTypeParams: ReadonlySet<Core.Name>) => ((name: Core.Name) => ((cx: t1) => ((g: Graph.Graph) => LibLogic.ifElse(LibLogic.or(LibSets.member(name)(boundVars))(LibSets.member(name)(inScopeTypeParams)))(({ tag: "right", value: null }))(LibLogic.ifElse(isLambdaBoundVariable(name))(({ tag: "right", value: null }))((() => {
  const schemaTypes = ((_x) => _x.schemaTypes)(g);
  return LibMaybes.cases(LibMaps.lookup(name)(schemaTypes))(({ tag: "right", value: null }))(((ts: Core.TypeScheme) => LibLogic.ifElse(LibLogic.not(LibLists.null_(((_x) => _x.variables)(ts))))(({ tag: "right", value: null }))((() => {
  const _m = Strip.deannotateType(((_x) => _x.type)(ts));
  switch (_m.tag) {
    case "record": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: null }))((_m as any).value);
    case "union": return ((_: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: null }))((_m as any).value);
    case "wrap": return ((_: Core.Type) => ({ tag: "right", value: null }))((_m as any).value);
    default: return ({ tag: "right", value: ((_x) => _x.type)(ts) })(_m);
  }
})())));
})())))))));
}

export function encodeVariable<t0>(env: JavaEnvironment.JavaEnvironment): ((x: Core.Name) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))) {
  return ((name: Core.Name) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const resolvedName = JavaUtils.lookupJavaVarName(aliases)(name);
  return (() => {
  const jid = JavaUtils.javaIdentifier(((_x) => _x)(resolvedName));
  return LibLogic.ifElse(LibSets.member(name)(((_x) => _x.branchVars)(aliases)))(({ tag: "right", value: JavaUtils.javaFieldAccessToJavaExpression(({
    qualifier: ({ tag: "primary", value: JavaUtils.javaExpressionToJavaPrimary(JavaUtils.javaIdentifierToJavaExpression(jid)) }),
    identifier: JavaUtils.javaIdentifier(JavaNames.valueFieldName)
  })) }))(LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)(LibStrings.cat([JavaNames.instanceName, "_", JavaNames.valueFieldName])))(isRecursiveVariable(aliases)(name)))((() => {
  const instanceExpr = JavaUtils.javaIdentifierToJavaExpression(JavaUtils.javaIdentifier(JavaNames.instanceName));
  return ({ tag: "right", value: JavaUtils.javaFieldAccessToJavaExpression(({
    qualifier: ({ tag: "primary", value: JavaUtils.javaExpressionToJavaPrimary(instanceExpr) }),
    identifier: JavaUtils.javaIdentifier(JavaNames.valueFieldName)
  })) });
})())(LibLogic.ifElse(LibLogic.and(isRecursiveVariable(aliases)(name))(LibLogic.not(isLambdaBoundIn(name)(((_x) => _x.lambdaVars)(aliases)))))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(({ tag: "left", value: ({
    qualifier: null,
    identifier: jid
  }) }))(JavaNames.getMethodName)([])) }))(LibLogic.ifElse(LibLogic.and(LibSets.member(name)(((_x) => _x.thunkedVars)(aliases)))(LibLogic.not(isLambdaBoundIn(name)(((_x) => _x.lambdaVars)(aliases)))))(({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(({ tag: "left", value: ({
    qualifier: null,
    identifier: jid
  }) }))(JavaNames.getMethodName)([])) }))(LibLogic.ifElse(isLambdaBoundIn(name)(((_x) => _x.lambdaVars)(aliases)))((() => {
  const actualName = findMatchingLambdaVar(name)(((_x) => _x.lambdaVars)(aliases));
  return (() => {
  const resolvedActual = JavaUtils.lookupJavaVarName(aliases)(actualName);
  return ({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(resolvedActual)) });
})();
})())(LibLogic.ifElse(LibSets.member(name)(((_x) => _x.inScopeJavaVars)(aliases)))(({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(elementJavaIdentifier(false)(false)(aliases)(resolvedName)) }))(LibEithers.bind(classifyDataReference(name)(cx)(g))(((cls: JavaEnvironment.JavaSymbolClass) => (() => {
  const _m = cls;
  switch (_m.tag) {
    case "hoistedLambda": return ((arity: number) => encodeVariable_hoistedLambdaCase(aliases)(name)(arity)(cx)(g))((_m as any).value);
    case "localVariable": return ((_: void) => ({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(elementJavaIdentifier(false)(false)(aliases)(resolvedName)) }))((_m as any).value);
    case "constant": return ((_: void) => ({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(elementJavaIdentifier(false)(false)(aliases)(name)) }))((_m as any).value);
    case "nullaryFunction": return ((_: void) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(null)(elementJavaIdentifier(false)(false)(aliases)(name))([])) }))((_m as any).value);
    case "unaryFunction": return ((_: void) => ({ tag: "right", value: JavaUtils.javaIdentifierToJavaExpression(elementJavaIdentifier(false)(true)(aliases)(name)) }))((_m as any).value);
  }
})()))))))));
})();
})();
})())));
}

export function encodeVariable_buildCurried(params: ReadonlyArray<Core.Name>): ((x: JavaSyntax.Expression) => JavaSyntax.Expression) {
  return ((inner: JavaSyntax.Expression) => LibLogic.ifElse(LibLists.null_(params))(inner)(JavaUtils.javaLambda(LibLists.head(params))(encodeVariable_buildCurried(LibLists.tail(params))(inner))));
}

export function encodeVariable_hoistedLambdaCase<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: number) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))) {
  return ((name: Core.Name) => ((arity: number) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const paramNames = LibLists.map(((i: number) => LibStrings.cat2("p")(LibLiterals.showInt32(i))))(LibMath.range(0)(LibMath.sub(arity)(1)));
  return (() => {
  const paramExprs = LibLists.map(((pn: Core.Name) => JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(pn))))(paramNames);
  return (() => {
  const call = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(null)(elementJavaIdentifier(false)(false)(aliases)(name))(paramExprs));
  return (() => {
  const lam = encodeVariable_buildCurried(paramNames)(call);
  return LibEithers.bind(({ tag: "right", value: Lexical.lookupBinding(g)(name) }))(((mel: Core.Binding | null) => LibMaybes.cases(mel)(({ tag: "right", value: lam }))(((el: Core.Binding) => LibMaybes.cases(((_x) => _x.type)(el))(({ tag: "right", value: lam }))(((ts: Core.TypeScheme) => (() => {
  const typ = ((_x) => _x.type)(ts);
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(typ)(cx)(g))(((jtype: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(rt)(JavaUtils.javaExpressionToJavaUnaryExpression(lam))) })))));
})()))))));
})();
})();
})();
})()))));
}

export function eqClause(tmpName: string): ((x: Core.FieldType) => JavaSyntax.InclusiveOrExpression) {
  return ((ft: Core.FieldType) => (() => {
  const fname = ((_x) => _x)(((_x) => _x.name)(ft));
  const ftype = ((_x) => _x.type)(ft);
  return LibLogic.ifElse(isBinaryType(ftype))(arraysEqualsClause(tmpName)(fname))(LibLogic.ifElse(isBigNumericType(ftype))(compareToZeroClause(tmpName)(fname))(equalsClause(tmpName)(fname)));
})());
}

export function equalsClause(tmpName: string): ((x: string) => JavaSyntax.InclusiveOrExpression) {
  return ((fname: string) => (() => {
  const thisArg = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression("this")(JavaUtils.javaIdentifier(fname)));
  const otherArg = JavaUtils.javaExpressionNameToJavaExpression(JavaUtils.fieldExpression(JavaUtils.javaIdentifier(tmpName))(JavaUtils.javaIdentifier(fname)));
  const header = ({ tag: "complex", value: ({
    variant: ({ tag: "type", value: JavaUtils.javaTypeName("java.util.Objects") }),
    typeArguments: [],
    identifier: JavaNames.equalsMethodName
  }) });
  return JavaUtils.javaPostfixExpressionToJavaInclusiveOrExpression(JavaUtils.javaMethodInvocationToJavaPostfixExpression(({
    header: header,
    arguments: [thisArg, otherArg]
  })));
})());
}

export function extractArgType<t0>(_lhs: t0): ((x: Core.Type) => Core.Type) {
  return ((typ: Core.Type) => (() => {
  const _m = typ;
  switch (_m.tag) {
    case "application": return ((at1: Core.ApplicationType) => (() => {
  const _m = ((_x) => _x.function)(at1);
  switch (_m.tag) {
    case "application": return ((_at2: Core.ApplicationType) => ((_x) => _x.argument)(at1))((_m as any).value);
    default: return typ(_m);
  }
})())((_m as any).value);
    default: return typ(_m);
  }
})());
}

export function extractDirectReturn(tparamSet: ReadonlySet<Core.Name>): ((x: Core.Type) => ReadonlyArray<readonly [Core.Name, Core.Name]>) {
  return ((t: Core.Type) => extractDirectReturn_go(tparamSet)(t));
}

export function extractDirectReturn_go(tparamSet: ReadonlySet<Core.Name>): ((x: Core.Type) => ReadonlyArray<readonly [Core.Name, Core.Name]>) {
  return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const dom = Strip.deannotateType(((_x) => _x.domain)(ft));
  return (() => {
  const cod = ((_x) => _x.codomain)(ft);
  return (() => {
  const _m = dom;
  switch (_m.tag) {
    case "variable": return ((inVar: Core.Name) => LibLogic.ifElse(LibSets.member(inVar)(tparamSet))((() => {
  const _m = Strip.deannotateType(cod);
  switch (_m.tag) {
    case "function": return ((ft2: Core.FunctionType) => (() => {
  const midArg = Strip.deannotateType(((_x) => _x.domain)(ft2));
  return (() => {
  const retPart = Strip.deannotateType(((_x) => _x.codomain)(ft2));
  return (() => {
  const _m = midArg;
  switch (_m.tag) {
    case "variable": return ((midVar: Core.Name) => LibLogic.ifElse(LibSets.member(midVar)(tparamSet))([])((() => {
  const _m = retPart;
  switch (_m.tag) {
    case "variable": return ((outVar: Core.Name) => LibLogic.ifElse(LibSets.member(outVar)(tparamSet))([[inVar, outVar]])([]))((_m as any).value);
    default: return [](_m);
  }
})()))((_m as any).value);
    default: return (() => {
  const _m = retPart;
  switch (_m.tag) {
    case "variable": return ((outVar: Core.Name) => LibLogic.ifElse(LibSets.member(outVar)(tparamSet))([[inVar, outVar]])([]))((_m as any).value);
    default: return [](_m);
  }
})()(_m);
  }
})();
})();
})())((_m as any).value);
    default: return [](_m);
  }
})())(extractDirectReturn_go(tparamSet)(cod)))((_m as any).value);
    default: return extractDirectReturn_go(tparamSet)(cod)(_m);
  }
})();
})();
})())((_m as any).value);
    default: return [](_m);
  }
})());
}

export function extractInOutPair(t: Core.Type): ReadonlyArray<readonly [Core.Name, Core.Name]> {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.domain)(ft));
  switch (_m.tag) {
    case "variable": return ((inVar: Core.Name) => (() => {
  const retType = unwrapReturnType(((_x) => _x.codomain)(ft));
  return (() => {
  const _m = Strip.deannotateType(retType);
  switch (_m.tag) {
    case "pair": return ((pt: Core.PairType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.first)(pt));
  switch (_m.tag) {
    case "variable": return ((outVar: Core.Name) => [[inVar, outVar]])((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})();
})())((_m as any).value);
    default: return [](_m);
  }
})())((_m as any).value);
    default: return [](_m);
  }
})();
}

export function extractTypeApplicationArgs(typ: Core.Type): ReadonlyArray<Core.Type> {
  return LibLists.reverse(extractTypeApplicationArgs_go(typ));
}

export function extractTypeApplicationArgs_go(t: Core.Type): ReadonlyArray<Core.Type> {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "application": return ((at: Core.ApplicationType) => LibLists.cons(((_x) => _x.argument)(at))(extractTypeApplicationArgs_go(((_x) => _x.function)(at))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function fieldTypeToFormalParam<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.FieldType) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.FormalParameter))) {
  return ((ft: Core.FieldType) => ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(((_x) => _x.type)(ft))(cx)(g))(((jt: JavaSyntax.Type) => ({ tag: "right", value: JavaUtils.javaTypeToJavaFormalParameter(jt)(((_x) => _x.name)(ft)) }))))));
}

export function filterByFlags<t0>(xs: ReadonlyArray<t0>): ((x: ReadonlyArray<boolean>) => ReadonlyArray<t0>) {
  return ((flags: ReadonlyArray<boolean>) => LibLists.map(((p: readonly [t0, boolean]) => LibPairs.first(p)))(LibLists.filter(((p: readonly [t0, boolean]) => LibPairs.second(p)))(LibLists.zip(xs)(flags))));
}

export function filterPhantomTypeArgs<t0, t1>(calleeName: Core.Name): ((x: ReadonlyArray<Core.Type>) => ((x: t0) => ((x: Graph.Graph) => t1 | ReadonlyArray<Core.Type>))) {
  return ((allTypeArgs: ReadonlyArray<Core.Type>) => ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(({ tag: "right", value: Lexical.lookupBinding(g)(calleeName) }))(((mel: Core.Binding | null) => LibMaybes.cases(mel)(({ tag: "right", value: allTypeArgs }))(((el: Core.Binding) => LibMaybes.cases(((_x) => _x.type)(el))(({ tag: "right", value: allTypeArgs }))(((ts: Core.TypeScheme) => (() => {
  const schemeVars = LibLists.filter(((v: Core.Name) => isSimpleName(v)))(((_x) => _x.variables)(ts));
  return (() => {
  const schemeTypeVars = collectTypeVars(((_x) => _x.type)(ts));
  return (() => {
  const schemeType = ((_x) => _x.type)(ts);
  return (() => {
  const nParams = countFunctionParams(schemeType);
  return (() => {
  const peeled = peelDomainTypes(nParams)(schemeType);
  return (() => {
  const calleeDoms = LibPairs.first(peeled);
  return (() => {
  const calleeCod = LibPairs.second(peeled);
  return (() => {
  const overgenSubst = detectAccumulatorUnification(calleeDoms)(calleeCod)(schemeVars);
  return (() => {
  const keepFlags = LibLists.map(((v: Core.Name) => LibLogic.and(LibSets.member(v)(schemeTypeVars))(LibLogic.not(LibMaps.member(v)(overgenSubst)))))(schemeVars);
  return LibLogic.ifElse(LibLogic.not(LibEquality.equal(LibLists.length(schemeVars))(LibLists.length(allTypeArgs))))(({ tag: "right", value: allTypeArgs }))(({ tag: "right", value: filterPhantomTypeArgs_filterAndApply(allTypeArgs)(keepFlags)(overgenSubst) }));
})();
})();
})();
})();
})();
})();
})();
})();
})())))))))));
}

export function filterPhantomTypeArgs_filterAndApply(allTypeArgs: ReadonlyArray<Core.Type>): ((x: ReadonlyArray<boolean>) => ((x: ReadonlyMap<Core.Name, Core.Type>) => ReadonlyArray<Core.Type>)) {
  return ((keepFlags: ReadonlyArray<boolean>) => ((overgenSubst: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const filtered = LibLists.map(((p: readonly [Core.Type, boolean]) => LibPairs.first(p)))(LibLists.filter(((p: readonly [Core.Type, boolean]) => LibPairs.second(p)))(LibLists.zip(allTypeArgs)(keepFlags)));
  return LibLogic.ifElse(LibLogic.not(LibMaps.null_(overgenSubst)))(LibLists.map(((t: Core.Type) => substituteTypeVarsWithTypes(overgenSubst)(t)))(filtered))(filtered);
})()));
}

export function findMatchingLambdaVar(name: Core.Name): ((x: ReadonlySet<Core.Name>) => Core.Name) {
  return ((lambdaVars: ReadonlySet<Core.Name>) => LibLogic.ifElse(LibSets.member(name)(lambdaVars))(name)(LibLogic.ifElse(isLambdaBoundIn_isQualified(name))(LibMaybes.fromMaybe(name)(LibLists.find(((lv: Core.Name) => LibLogic.and(isLambdaBoundIn_isQualified(lv))(LibEquality.equal(Names.localNameOf(lv))(Names.localNameOf(name)))))(LibSets.toList(lambdaVars))))(LibLogic.ifElse(LibSets.member(Names.localNameOf(name))(lambdaVars))(Names.localNameOf(name))(name))));
}

export function findPairFirst(t: Core.Type): Core.Name | null {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "pair": return ((pt: Core.PairType) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.first)(pt));
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => v)((_m as any).value);
    default: return null(_m);
  }
})())((_m as any).value);
    default: return null(_m);
  }
})();
}

export function findSelfRefVar<t0>(grouped: ReadonlyMap<t0, ReadonlyArray<t0>>): t0 | null {
  return (() => {
  const selfRefs = LibLists.filter(((entry: readonly [t0, ReadonlyArray<t0>]) => LibLists.elem(LibPairs.first(entry))(LibPairs.second(entry))))(LibMaps.toList(grouped));
  return LibLogic.ifElse(LibLists.null_(selfRefs))(null)(LibPairs.first(LibLists.head(selfRefs)));
})();
}

export const first20Primes: ReadonlyArray<bigint> = [2n, 3n, 5n, 7n, 11n, 13n, 17n, 19n, 23n, 29n, 31n, 37n, 41n, 43n, 47n, 53n, 59n, 61n, 67n, 71n];

export function flattenApps(t: Core.Term): ((x: ReadonlyArray<Core.Term>) => readonly [ReadonlyArray<Core.Term>, Core.Term]) {
  return ((acc: ReadonlyArray<Core.Term>) => (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => flattenApps(((_x) => _x.function)(app))(LibLists.cons(((_x) => _x.argument)(app))(acc)))((_m as any).value);
    default: return [acc, t](_m);
  }
})());
}

export function flattenBindings(bindings: ReadonlyArray<Core.Binding>): ReadonlyArray<Core.Binding> {
  return LibLists.bind(bindings)(((b: Core.Binding) => (() => {
  const _m = Strip.deannotateTerm(((_x) => _x.term)(b));
  switch (_m.tag) {
    case "let": return ((lt: Core.Let) => LibLists.concat2(flattenBindings(((_x) => _x.bindings)(lt)))([({
    name: ((_x) => _x.name)(b),
    term: ((_x) => _x.body)(lt),
    type: ((_x) => _x.type)(b)
  })]))((_m as any).value);
    default: return [b](_m);
  }
})()));
}

export function freshJavaName(base: Core.Name): ((x: ReadonlySet<Core.Name>) => Core.Name) {
  return ((avoid: ReadonlySet<Core.Name>) => freshJavaName_go(base)(avoid)(2));
}

export function freshJavaName_go(base: Core.Name): ((x: ReadonlySet<Core.Name>) => ((x: number) => Core.Name)) {
  return ((avoid: ReadonlySet<Core.Name>) => ((i: number) => (() => {
  const candidate = LibStrings.cat2(((_x) => _x)(base))(LibLiterals.showInt32(i));
  return LibLogic.ifElse(LibSets.member(candidate)(avoid))(freshJavaName_go(base)(avoid)(LibMath.add(i)(1)))(candidate);
})()));
}

export function functionCall(env: JavaEnvironment.JavaEnvironment): ((x: boolean) => ((x: Core.Name) => ((x: ReadonlyArray<Core.Term>) => ((x: ReadonlyArray<Core.Type>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))))) {
  return ((isPrim: boolean) => ((name: Core.Name) => ((args: ReadonlyArray<Core.Term>) => ((typeApps: ReadonlyArray<Core.Type>) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return (() => {
  const isLambdaBound = isLambdaBoundIn(name)(((_x) => _x.lambdaVars)(aliases));
  return LibEithers.bind(LibEithers.mapList(((arg: Core.Term) => encodeTerm(env)(arg)(cx)(g)))(args))(((jargs0: ReadonlyArray<JavaSyntax.Expression>) => (() => {
  const wrapResult = wrapLazyArguments(name)(jargs0);
  return (() => {
  const jargs = LibPairs.first(wrapResult);
  return (() => {
  const mMethodOverride = LibPairs.second(wrapResult);
  return LibLogic.ifElse(LibLogic.or(isLocalVariable(name))(isLambdaBound))(LibEithers.bind(encodeVariable(env)(name)(cx)(g))(((baseExpr: JavaSyntax.Expression) => ({ tag: "right", value: LibLists.foldl(((acc: JavaSyntax.Expression) => ((jarg: JavaSyntax.Expression) => applyJavaArg(acc)(jarg))))(baseExpr)(jargs) }))))((() => {
  const overrideMethodName = ((jid: JavaSyntax.Identifier) => LibMaybes.cases(mMethodOverride)(jid)(((m: string) => (() => {
  const s = ((_x) => _x)(jid);
  return LibStrings.cat2(LibStrings.fromList(LibLists.take(LibMath.sub(LibStrings.length(s))(LibStrings.length(JavaNames.applyMethodName)))(LibStrings.toList(s))))(m);
})())));
  return LibLogic.ifElse(LibLists.null_(typeApps))((() => {
  const header = ({ tag: "simple", value: overrideMethodName(elementJavaIdentifier(isPrim)(false)(aliases)(name)) });
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(({
    header: header,
    arguments: jargs
  })) });
})())((() => {
  const qn = Names.qualifyName(name);
  return (() => {
  const mns = ((_x) => _x.namespace)(qn);
  return (() => {
  const localName = ((_x) => _x.local)(qn);
  return LibMaybes.cases(mns)((() => {
  const header = ({ tag: "simple", value: overrideMethodName(elementJavaIdentifier(isPrim)(false)(aliases)(name)) });
  return ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(({
    header: header,
    arguments: jargs
  })) });
})())(((ns_: Packaging.Namespace) => (() => {
  const classId = JavaUtils.nameToJavaName(aliases)(elementsQualifiedName(ns_));
  return (() => {
  const methodId = LibLogic.ifElse(isPrim)(overrideMethodName(LibStrings.cat2(((_x) => _x)(JavaUtils.nameToJavaName(aliases)(Names.unqualifyName(({
    namespace: ns_,
    local: Formatting.capitalize(localName)
  })))))(LibStrings.cat2(".")(JavaNames.applyMethodName))))(JavaUtils.sanitizeJavaName(localName));
  return LibEithers.bind(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: ({ tag: "reference", value: rt }) })))))))(typeApps))(((jTypeArgs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(jargs)) })));
})();
})()));
})();
})();
})());
})());
})();
})();
})()));
})();
})()))))));
}

export function getCodomain<t0>(ann: ReadonlyMap<Core.Name, Core.Term>): ((x: t0) => ((x: Graph.Graph) => Errors.Error | Core.Type)) {
  return ((cx: t0) => ((g: Graph.Graph) => LibEithers.map(((ft: Core.FunctionType) => ((_x) => _x.codomain)(ft)))(getFunctionType(ann)(cx)(g))));
}

export function getFunctionType<t0>(ann: ReadonlyMap<Core.Name, Core.Term>): ((x: t0) => ((x: Graph.Graph) => Errors.Error | Core.FunctionType)) {
  return ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(LibEithers.bimap(((__de: Errors.DecodingError) => ({ tag: "other", value: ((_x) => _x)(__de) })))(((__a: Core.Type | null) => __a))(Annotations.getType(g)(ann)))(((mt: Core.Type | null) => LibMaybes.cases(mt)(({ tag: "left", value: ({ tag: "other", value: "type annotation is required for function and elimination terms in Java" }) }))(((t: Core.Type) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => ({ tag: "right", value: ft }))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("expected function type, got: ")(ShowCore.type(t)) }) })(_m);
  }
})()))))));
}

export function groupPairsByFirst<t0, t1>(pairs: ReadonlyArray<readonly [t0, t1]>): ReadonlyMap<t0, ReadonlyArray<t1>> {
  return LibLists.foldl(((m: ReadonlyMap<t0, ReadonlyArray<t1>>) => ((p: readonly [t0, t1]) => (() => {
  const k = LibPairs.first(p);
  return (() => {
  const v = LibPairs.second(p);
  return LibMaps.alter(((mv: ReadonlyArray<t1> | null) => LibMaybes.maybe([v])(((vs: ReadonlyArray<t1>) => LibLists.concat2(vs)([v])))(mv)))(k)(m);
})();
})())))(LibMaps.empty)(pairs);
}

export function hashCodeCompareExpr(otherVar: string): ((x: string) => JavaSyntax.Expression) {
  return ((fname: string) => (() => {
  const header = ({ tag: "complex", value: ({
    variant: ({ tag: "type", value: JavaUtils.javaTypeName("Integer") }),
    typeArguments: [],
    identifier: "compare"
  }) });
  const thisHashCode = JavaUtils.javaMethodInvocationToJavaExpression(({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "expression", value: ({
    qualifier: null,
    identifier: JavaUtils.sanitizeJavaName(fname)
  }) }),
    typeArguments: [],
    identifier: JavaNames.hashCodeMethodName
  }) }),
    arguments: []
  }));
  const otherHashCode = JavaUtils.javaMethodInvocationToJavaExpression(({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "expression", value: JavaUtils.fieldExpression(JavaUtils.javaIdentifier(otherVar))(JavaUtils.javaIdentifier(fname)) }),
    typeArguments: [],
    identifier: JavaNames.hashCodeMethodName
  }) }),
    arguments: []
  }));
  return JavaUtils.javaMethodInvocationToJavaExpression(({
    header: header,
    arguments: [thisHashCode, otherHashCode]
  }));
})());
}

export function hashCodeMultPair(i: bigint): ((x: Core.Name) => JavaSyntax.MultiplicativeExpression) {
  return ((fname: Core.Name) => (() => {
  const fnameStr = ((_x) => _x)(fname);
  const lhs = ({ tag: "unary", value: JavaUtils.javaPrimaryToJavaUnaryExpression(JavaUtils.javaLiteralToJavaPrimary(JavaUtils.javaInt(i))) });
  const rhs = JavaUtils.javaPostfixExpressionToJavaUnaryExpression(JavaUtils.javaMethodInvocationToJavaPostfixExpression(({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "type", value: JavaUtils.javaTypeName("java.util.Objects") }),
    typeArguments: [],
    identifier: JavaNames.hashCodeMethodName
  }) }),
    arguments: [JavaUtils.javaExpressionNameToJavaExpression(({
    qualifier: null,
    identifier: JavaUtils.sanitizeJavaName(fnameStr)
  }))]
  })));
  return ({ tag: "times", value: ({
    lhs: lhs,
    rhs: rhs
  }) });
})());
}

export function innerClassRef(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: string) => JavaSyntax.Identifier)) {
  return ((name: Core.Name) => ((local: string) => (() => {
  const id = ((_x) => _x)(JavaUtils.nameToJavaName(aliases)(name));
  return LibStrings.cat2(LibStrings.cat2(id)("."))(local);
})()));
}

export function insertBranchVar(name: Core.Name): ((x: JavaEnvironment.JavaEnvironment) => JavaEnvironment.JavaEnvironment) {
  return ((env: JavaEnvironment.JavaEnvironment) => (() => {
  const aliases = ((_x) => _x.aliases)(env);
  return ({
    aliases: ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases),
    packages: ((_x) => _x.packages)(aliases),
    branchVars: LibSets.insert(name)(((_x) => _x.branchVars)(aliases)),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases),
    inScopeJavaVars: ((_x) => _x.inScopeJavaVars)(aliases),
    varRenames: ((_x) => _x.varRenames)(aliases),
    lambdaVars: ((_x) => _x.lambdaVars)(aliases),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases),
    methodCodomain: null,
    thunkedVars: ((_x) => _x.thunkedVars)(aliases)
  }),
    graph: ((_x) => _x.graph)(env)
  });
})());
}

export function interfaceTypes(isSer: boolean): ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ReadonlyArray<JavaSyntax.InterfaceType>))) {
  return ((aliases: JavaEnvironment.Aliases) => ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => (() => {
  const javaSerializableType = ({
    annotations: [],
    qualifier: ({ tag: "none" }),
    identifier: JavaUtils.javaTypeIdentifier("Serializable"),
    arguments: []
  });
  const selfTypeArg = ({ tag: "reference", value: JavaUtils.nameToJavaReferenceType(aliases)(false)(LibLists.map(((tp_: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToTypeArgument(tp_)))(tparams))(elName)(null) });
  const javaComparableType = ({
    annotations: [],
    qualifier: ({ tag: "none" }),
    identifier: JavaUtils.javaTypeIdentifier("Comparable"),
    arguments: [selfTypeArg]
  });
  return LibLogic.ifElse(isSer)([javaSerializableType, javaComparableType])([]);
})())));
}

export function isBigNumericType(typ: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "float": return ((ft: Core.FloatType) => (() => {
  const _m = ft;
  switch (_m.tag) {
    case "bigfloat": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    case "integer": return ((it: Core.IntegerType) => (() => {
  const _m = it;
  switch (_m.tag) {
    case "bigint": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isBinaryType(typ: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isFieldUnitType<t0, t1>(typeName: Core.Name): ((x: Core.Name) => ((x: t0) => ((x: Graph.Graph) => t1 | boolean))) {
  return ((fieldName: Core.Name) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const schemaTypes = ((_x) => _x.schemaTypes)(g);
  return LibMaybes.cases(LibMaps.lookup(typeName)(schemaTypes))(({ tag: "right", value: false }))(((ts: Core.TypeScheme) => (() => {
  const _m = Strip.deannotateType(((_x) => _x.type)(ts));
  switch (_m.tag) {
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => ({ tag: "right", value: LibMaybes.cases(LibLists.find(((ft: Core.FieldType) => LibEquality.equal(((_x) => _x.name)(ft))(fieldName)))(rt))(false)(((ft: Core.FieldType) => Predicates.isUnitType(Strip.deannotateType(((_x) => _x.type)(ft))))) }))((_m as any).value);
    default: return ({ tag: "right", value: false })(_m);
  }
})()));
})())));
}

export function isLambdaBoundIn(name: Core.Name): ((x: ReadonlySet<Core.Name>) => boolean) {
  return ((lambdaVars: ReadonlySet<Core.Name>) => LibLogic.or(LibSets.member(name)(lambdaVars))(LibLogic.or(LibLogic.and(isLambdaBoundIn_isQualified(name))(LibMaybes.isJust(LibLists.find(((lv: Core.Name) => LibLogic.and(isLambdaBoundIn_isQualified(lv))(LibEquality.equal(Names.localNameOf(lv))(Names.localNameOf(name)))))(LibSets.toList(lambdaVars)))))(LibLogic.and(LibLogic.not(isLambdaBoundIn_isQualified(name)))(LibSets.member(Names.localNameOf(name))(lambdaVars)))));
}

export function isLambdaBoundIn_isQualified(n: Core.Name): boolean {
  return LibMaybes.isJust(((_x) => _x.namespace)(Names.qualifyName(n)));
}

export function isLambdaBoundVariable(name: Core.Name): boolean {
  return (() => {
  const v = ((_x) => _x)(name);
  return LibEquality.lte(LibStrings.length(v))(4);
})();
}

export function isLocalVariable(name: Core.Name): boolean {
  return LibMaybes.isNothing(((_x) => _x.namespace)(Names.qualifyName(name)));
}

export function isNonComparableType(typ: Core.Type): boolean {
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "either": return ((_: Core.EitherType) => true)((_m as any).value);
    case "function": return ((_: Core.FunctionType) => true)((_m as any).value);
    case "unit": return ((_: void) => true)((_m as any).value);
    case "literal": return ((lt: Core.LiteralType) => (() => {
  const _m = lt;
  switch (_m.tag) {
    case "binary": return ((_: void) => true)((_m as any).value);
    default: return false(_m);
  }
})())((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => isNonComparableType(((_x) => _x.body)(ft)))((_m as any).value);
    default: return false(_m);
  }
})();
}

export function isRecursiveVariable(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => boolean) {
  return ((name: Core.Name) => LibSets.member(name)(((_x) => _x.recursiveVars)(aliases)));
}

export function isSerializableJavaType(typ: Core.Type): boolean {
  return Predicates.isNominalType(typ);
}

export function isSimpleName(name: Core.Name): boolean {
  return LibEquality.equal(LibLists.length(LibStrings.splitOn(".")(((_x) => _x)(name))))(1);
}

export function isUnresolvedInferenceVar(name: Core.Name): boolean {
  return (() => {
  const chars = LibStrings.toList(((_x) => _x)(name));
  return LibLogic.ifElse(LibLists.null_(chars))(false)(LibLogic.ifElse(LibLogic.not(LibEquality.equal(LibLists.head(chars))(116)))(false)((() => {
  const rest = LibLists.tail(chars);
  return LibLogic.and(LibLogic.not(LibLists.null_(rest)))(LibLists.null_(LibLists.filter(((c: number) => LibLogic.not(isUnresolvedInferenceVar_isDigit(c))))(rest)));
})()));
})();
}

export function isUnresolvedInferenceVar_isDigit(c: number): boolean {
  return LibLogic.and(LibEquality.gte(c)(48))(LibEquality.lte(c)(57));
}

export const java11Features: JavaEnvironment.JavaFeatures = ({
    supportsDiamondOperator: true
  });

export const java8Features: JavaEnvironment.JavaFeatures = ({
    supportsDiamondOperator: false
  });

export const javaComparableRefType: JavaSyntax.ReferenceType = ({ tag: "classOrInterface", value: ({ tag: "class", value: ({
    annotations: [],
    qualifier: ({ tag: "none" }),
    identifier: JavaUtils.javaTypeIdentifier("Comparable"),
    arguments: []
  }) }) });

export function javaEnvGetGraph(env: JavaEnvironment.JavaEnvironment): Graph.Graph {
  return ((_x) => _x.graph)(env);
}

export function javaEnvSetGraph(g: Graph.Graph): ((x: JavaEnvironment.JavaEnvironment) => JavaEnvironment.JavaEnvironment) {
  return ((env: JavaEnvironment.JavaEnvironment) => ({
    aliases: ((_x) => _x.aliases)(env),
    graph: g
  }));
}

export const javaFeatures: JavaEnvironment.JavaFeatures = java11Features;

export function javaIdentifierToString(id: JavaSyntax.Identifier): string {
  return ((_x) => _x)(id);
}

export function javaTypeArgumentsForNamedType<t0>(tname: Core.Name): ((x: t0) => ((x: Graph.Graph) => Errors.Error | ReadonlyArray<JavaSyntax.TypeArgument>)) {
  return ((cx: t0) => ((g: Graph.Graph) => LibEithers.bind(Resolution.requireType(cx)(g)(tname))(((typ: Core.Type) => ({ tag: "right", value: LibLists.map(((tp_: JavaSyntax.TypeParameter) => JavaUtils.typeParameterToTypeArgument(tp_)))(javaTypeParametersForType(typ)) })))));
}

export function javaTypeArgumentsForType(typ: Core.Type): ReadonlyArray<JavaSyntax.TypeArgument> {
  return LibLists.reverse(LibLists.map(JavaUtils.typeParameterToTypeArgument)(javaTypeParametersForType(typ)));
}

export function javaTypeParametersForType(typ: Core.Type): ReadonlyArray<JavaSyntax.TypeParameter> {
  return (() => {
  const toParam = ((name: Core.Name) => JavaUtils.javaTypeParameter(Formatting.capitalize(((_x) => _x)(name))));
  const boundVars = javaTypeParametersForType_bvars(typ);
  const freeVars = LibLists.filter(((v: Core.Name) => isLambdaBoundVariable(v)))(LibSets.toList(Variables.freeVariablesInType(typ)));
  const vars = LibLists.nub(LibLists.concat2(boundVars)(freeVars));
  return LibLists.map(toParam)(vars);
})();
}

export function javaTypeParametersForType_bvars(t: Core.Type): ReadonlyArray<Core.Name> {
  return (() => {
  const _m = t;
  switch (_m.tag) {
    case "forall": return ((ft: Core.ForallType) => LibLists.cons(((_x) => _x.parameter)(ft))(javaTypeParametersForType_bvars(((_x) => _x.body)(ft))))((_m as any).value);
    default: return [](_m);
  }
})();
}

export function moduleToJava(mod: Packaging.Module): ((x: ReadonlyArray<Packaging.Definition>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | ReadonlyMap<string, string>))) {
  return ((defs: ReadonlyArray<Packaging.Definition>) => ((cx: Context.Context) => ((g: Graph.Graph) => LibEithers.bind(encodeDefinitions(mod)(defs)(cx)(g))(((units: ReadonlyMap<Core.Name, JavaSyntax.CompilationUnit>) => ({ tag: "right", value: LibMaps.fromList(LibLists.map(((entry: readonly [Core.Name, JavaSyntax.CompilationUnit]) => (() => {
  const name = LibPairs.first(entry);
  return (() => {
  const unit = LibPairs.second(entry);
  return [bindingNameToFilePath(name), Serialization.printExpr(Serialization.parenthesize(JavaSerde.writeCompilationUnit(unit)))];
})();
})()))(LibMaps.toList(units))) }))))));
}

export function nameMapToTypeMap<t0>(m: ReadonlyMap<t0, Core.Name>): ReadonlyMap<t0, Core.Type> {
  return LibMaps.map(((v: Core.Name) => ({ tag: "variable", value: v })))(m);
}

export function namespaceParent(ns: Packaging.Namespace): Packaging.Namespace | null {
  return (() => {
  const parts = LibStrings.splitOn(".")(((_x) => _x)(ns));
  return LibLogic.ifElse(LibLists.null_(LibLists.init(parts)))(null)(LibStrings.intercalate(".")(LibLists.init(parts)));
})();
}

export function needsThunking(t: Core.Term): boolean {
  return (() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "let": return ((_lt: Core.Let) => true)((_m as any).value);
    case "typeApplication": return ((_ta: Core.TypeApplicationTerm) => true)((_m as any).value);
    case "typeLambda": return ((_tl: Core.TypeLambda) => true)((_m as any).value);
    default: return LibLists.foldl(((b: boolean) => ((st: Core.Term) => LibLogic.or(b)(needsThunking(st)))))(false)(Rewriting.subterms(t))(_m);
  }
})();
}

export function noComment(decl: JavaSyntax.ClassBodyDeclaration): JavaSyntax.ClassBodyDeclarationWithComments {
  return ({
    value: decl,
    comments: null
  });
}

export function otherwiseBranch(env: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: Core.Type) => ((x: Core.Type) => ((x: Core.Name) => ((x: JavaSyntax.Type) => ((x: ReadonlyArray<JavaSyntax.TypeArgument>) => ((x: Core.Term) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments))))))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((dom: Core.Type) => ((cod: Core.Type) => ((tname: Core.Name) => ((jcod: JavaSyntax.Type) => ((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ((d: Core.Term) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const jdom = ({ tag: "reference", value: JavaUtils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(null) });
  return (() => {
  const mods = [({ tag: "public" })];
  return (() => {
  const anns = [JavaUtils.overrideAnnotation];
  return (() => {
  const param = JavaUtils.javaTypeToJavaFormalParameter(jdom)("instance");
  return (() => {
  const result = ({ tag: "type", value: jcod });
  return LibEithers.bind(analyzeJavaFunction(env)(d)(cx)(g))(((fs: Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>) => (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const rawBody = ((_x) => _x.body)(fs);
  return (() => {
  const innerBody = annotateBodyWithCod(cod)(rawBody);
  return (() => {
  const env2 = ((_x) => _x.environment)(fs);
  return LibEithers.bind(bindingsToStatements(env2)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env3 = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTerm(env3)(innerBody)(cx)(g))(((jret: JavaSyntax.Expression) => (() => {
  const returnStmt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jret) });
  return (() => {
  const allStmts = LibLists.concat2(bindingStmts)([returnStmt]);
  return ({ tag: "right", value: noComment(JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.otherwiseMethodName)([param])(result)(allStmts)) });
})();
})()));
})();
})()));
})();
})();
})();
})()));
})();
})();
})();
})();
})())))))))));
}

export function peelDomainTypes(n: number): ((x: Core.Type) => readonly [ReadonlyArray<Core.Type>, Core.Type]) {
  return ((t: Core.Type) => LibLogic.ifElse(LibEquality.lte(n)(0))([[], t])((() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const rest = peelDomainTypes(LibMath.sub(n)(1))(((_x) => _x.codomain)(ft));
  return [LibLists.cons(((_x) => _x.domain)(ft))(LibPairs.first(rest)), LibPairs.second(rest)];
})())((_m as any).value);
    default: return [[], t](_m);
  }
})()));
}

export function peelDomainsAndCod(n: number): ((x: Core.Type) => readonly [ReadonlyArray<Core.Type>, Core.Type]) {
  return ((t: Core.Type) => LibLogic.ifElse(LibEquality.lte(n)(0))([[], t])((() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const rest = peelDomainsAndCod(LibMath.sub(n)(1))(((_x) => _x.codomain)(ft));
  return [LibLists.cons(((_x) => _x.domain)(ft))(LibPairs.first(rest)), LibPairs.second(rest)];
})())((_m as any).value);
    default: return [[], t](_m);
  }
})()));
}

export function peelExpectedTypes(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: number) => ((x: Core.Type) => ReadonlyArray<Core.Type>)) {
  return ((n: number) => ((t: Core.Type) => LibLogic.ifElse(LibEquality.equal(n)(0))([])((() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => LibLists.cons(applySubstFull(subst)(((_x) => _x.domain)(ft)))(peelExpectedTypes(subst)(LibMath.sub(n)(1))(((_x) => _x.codomain)(ft))))((_m as any).value);
    default: return [](_m);
  }
})())));
}

export function propagateType(typ: Core.Type): ((x: Core.Term) => Core.Term) {
  return ((term: Core.Term) => (() => {
  const setTypeAnn = ((t: Core.Term) => Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(typ))(t));
  return (() => {
  const _m = Strip.deannotateTerm(term);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => (() => {
  const annotated = setTypeAnn(term);
  return (() => {
  const _m = Strip.deannotateType(typ);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => propagateType_propagateIntoLambda(((_x) => _x.codomain)(ft))(annotated))((_m as any).value);
    default: return annotated(_m);
  }
})();
})())((_m as any).value);
    case "let": return ((lt: Core.Let) => (() => {
  const propagatedBindings = LibLists.map(((b: Core.Binding) => LibMaybes.maybe(b)(((ts: Core.TypeScheme) => ({
    name: ((_x) => _x.name)(b),
    term: propagateType(((_x) => _x.type)(ts))(((_x) => _x.term)(b)),
    type: ((_x) => _x.type)(b)
  })))(((_x) => _x.type)(b))))(((_x) => _x.bindings)(lt));
  return setTypeAnn(propagateType_rebuildLet(term)(propagatedBindings)(propagateType(typ)(((_x) => _x.body)(lt))));
})())((_m as any).value);
    case "application": return ((app: Core.Application) => (() => {
  const fun = ((_x) => _x.function)(app);
  return (() => {
  const arg = ((_x) => _x.argument)(app);
  return (() => {
  const annotatedFun = (() => {
  const _m = Strip.deannotateTerm(fun);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const dom = Resolution.nominalApplication(((_x) => _x.typeName)(cs))([]);
  return (() => {
  const ft = ({ tag: "function", value: ({
    domain: dom,
    codomain: typ
  }) });
  return Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(ft))(fun);
})();
})())((_m as any).value);
    default: return fun(_m);
  }
})();
  return setTypeAnn(({ tag: "application", value: ({
    function: annotatedFun,
    argument: arg
  }) }));
})();
})();
})())((_m as any).value);
    default: return setTypeAnn(term)(_m);
  }
})();
})());
}

export function propagateType_propagateIntoLambda(cod: Core.Type): ((x: Core.Term) => Core.Term) {
  return ((t: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: propagateType_propagateIntoLambda(cod)(((_x) => _x.body)(at)),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "lambda": return ((lam: Core.Lambda) => ({ tag: "lambda", value: ({
    parameter: ((_x) => _x.parameter)(lam),
    domain: ((_x) => _x.domain)(lam),
    body: propagateType(cod)(((_x) => _x.body)(lam))
  }) }))((_m as any).value);
    default: return t(_m);
  }
})());
}

export function propagateType_rebuildLet(t: Core.Term): ((x: ReadonlyArray<Core.Binding>) => ((x: Core.Term) => Core.Term)) {
  return ((bindings: ReadonlyArray<Core.Binding>) => ((newBody: Core.Term) => (() => {
  const _m = t;
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => ({ tag: "annotated", value: ({
    body: propagateType_rebuildLet(((_x) => _x.body)(at))(bindings)(newBody),
    annotation: ((_x) => _x.annotation)(at)
  }) }))((_m as any).value);
    case "let": return ((_lt: Core.Let) => ({ tag: "let", value: ({
    bindings: bindings,
    body: newBody
  }) }))((_m as any).value);
    default: return t(_m);
  }
})()));
}

export function propagateTypesInAppChain(fixedCod: Core.Type): ((x: Core.Type) => ((x: Core.Term) => Core.Term)) {
  return ((resultType: Core.Type) => ((t: Core.Term) => (() => {
  const flattened = flattenApps(t)([]);
  return (() => {
  const args = LibPairs.first(flattened);
  return (() => {
  const fun = LibPairs.second(flattened);
  return (() => {
  const lambdaDomsResult = collectLambdaDomains(fun);
  return (() => {
  const lambdaDoms = LibPairs.first(lambdaDomsResult);
  return (() => {
  const nArgs = LibLists.length(args);
  return (() => {
  const nLambdaDoms = LibLists.length(lambdaDoms);
  return LibLogic.ifElse(LibLogic.and(LibEquality.gt(nLambdaDoms)(0))(LibEquality.gt(nArgs)(0)))((() => {
  const bodyRetType = LibPairs.second(peelDomainsAndCod(LibMath.sub(nLambdaDoms)(nArgs))(resultType));
  return (() => {
  const funType = LibLists.foldl(((c: Core.Type) => ((d: Core.Type) => ({ tag: "function", value: ({
    domain: d,
    codomain: c
  }) }))))(bodyRetType)(LibLists.reverse(lambdaDoms));
  return (() => {
  const annotatedFun = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(funType))(fun);
  return rebuildApps(annotatedFun)(args)(funType);
})();
})();
})())((() => {
  const _m = Strip.deannotateTerm(t);
  switch (_m.tag) {
    case "application": return ((app: Core.Application) => (() => {
  const lhs = ((_x) => _x.function)(app);
  return (() => {
  const rhs = ((_x) => _x.argument)(app);
  return (() => {
  const annotatedLhs = (() => {
  const _m = Strip.deannotateTerm(lhs);
  switch (_m.tag) {
    case "cases": return ((cs: Core.CaseStatement) => (() => {
  const dom = Resolution.nominalApplication(((_x) => _x.typeName)(cs))([]);
  return (() => {
  const ft = ({ tag: "function", value: ({
    domain: dom,
    codomain: fixedCod
  }) });
  return Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(ft))(lhs);
})();
})())((_m as any).value);
    default: return lhs(_m);
  }
})();
  return Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(resultType))(({ tag: "application", value: ({
    function: annotatedLhs,
    argument: rhs
  }) }));
})();
})();
})())((_m as any).value);
    default: return Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(resultType))(t)(_m);
  }
})());
})();
})();
})();
})();
})();
})();
})()));
}

export function rebuildApps(f: Core.Term): ((x: ReadonlyArray<Core.Term>) => ((x: Core.Type) => Core.Term)) {
  return ((args: ReadonlyArray<Core.Term>) => ((fType: Core.Type) => LibLogic.ifElse(LibLists.null_(args))(f)((() => {
  const _m = Strip.deannotateType(fType);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => (() => {
  const arg = LibLists.head(args);
  return (() => {
  const rest = LibLists.tail(args);
  return (() => {
  const remainingType = ((_x) => _x.codomain)(ft);
  return (() => {
  const app = ({ tag: "application", value: ({
    function: f,
    argument: arg
  }) });
  return (() => {
  const annotatedApp = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(remainingType))(app);
  return rebuildApps(annotatedApp)(rest)(remainingType);
})();
})();
})();
})();
})())((_m as any).value);
    default: return LibLists.foldl(((acc: Core.Term) => ((a: Core.Term) => ({ tag: "application", value: ({
    function: acc,
    argument: a
  }) }))))(f)(args)(_m);
  }
})())));
}

export function recordCompareToMethod<t0>(aliases: JavaEnvironment.Aliases): ((x: t0) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => JavaSyntax.ClassBodyDeclaration))) {
  return ((tparams: t0) => ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const anns = [JavaUtils.overrideAnnotation, JavaUtils.suppressWarningsUncheckedAnnotation];
  const mods = [({ tag: "public" })];
  const param = JavaUtils.javaTypeToJavaFormalParameter(JavaUtils.javaTypeFromTypeName(aliases)(elName))(JavaNames.otherInstanceName);
  const result = JavaUtils.javaTypeToJavaResult(JavaUtils.javaIntType);
  return JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.compareToMethodName)([param])(result)(compareToBody(aliases)(JavaNames.otherInstanceName)(fields));
})())));
}

export function recordConstructor<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclaration)))) {
  return ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const assignStmts = LibLists.map(((f: Core.FieldType) => ({ tag: "statement", value: JavaUtils.toAssignStmt(((_x) => _x.name)(f)) })))(fields);
  return LibEithers.bind(LibEithers.mapList(((f: Core.FieldType) => fieldTypeToFormalParam(aliases)(f)(cx)(g)))(fields))(((params: ReadonlyArray<JavaSyntax.FormalParameter>) => ({ tag: "right", value: JavaUtils.makeConstructor(aliases)(elName)(false)(params)(assignStmts) })));
})()))));
}

export function recordEqualsMethod(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => JavaSyntax.ClassBodyDeclaration)) {
  return ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const anns = [JavaUtils.overrideAnnotation];
  const mods = [({ tag: "public" })];
  const param = JavaUtils.javaTypeToJavaFormalParameter(JavaUtils.javaRefType([])(null)("Object"))(JavaNames.otherInstanceName);
  const result = JavaUtils.javaTypeToJavaResult(JavaUtils.javaBooleanType);
  const tmpName = "o";
  const instanceOfStmt = ({ tag: "statement", value: ({ tag: "ifThen", value: ({
    expression: JavaUtils.javaUnaryExpressionToJavaExpression(({ tag: "other", value: ({ tag: "not", value: JavaUtils.javaRelationalExpressionToJavaUnaryExpression(JavaUtils.javaInstanceOf(JavaUtils.javaIdentifierToJavaRelationalExpression(JavaUtils.javaIdentifier(JavaNames.otherInstanceName)))(JavaUtils.nameToJavaReferenceType(aliases)(false)([])(elName)(null))) }) })),
    statement: JavaUtils.javaReturnStatement(JavaUtils.javaBooleanExpression(false))
  }) }) });
  const castStmt = JavaUtils.variableDeclarationStatement(aliases)(JavaUtils.javaTypeFromTypeName(aliases)(elName))(JavaUtils.javaIdentifier(tmpName))(JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(JavaUtils.nameToJavaReferenceType(aliases)(false)([])(elName)(null))(JavaUtils.javaIdentifierToJavaUnaryExpression(JavaUtils.sanitizeJavaName(JavaNames.otherInstanceName)))));
  const returnAllFieldsEqual = ({ tag: "statement", value: JavaUtils.javaReturnStatement(LibLogic.ifElse(LibLists.null_(fields))(JavaUtils.javaBooleanExpression(true))(JavaUtils.javaConditionalAndExpressionToJavaExpression(LibLists.map(((f: Core.FieldType) => eqClause(tmpName)(f)))(fields)))) });
  return JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.equalsMethodName)([param])(result)([instanceOfStmt, castStmt, returnAllFieldsEqual]);
})()));
}

export function recordHashCodeMethod(fields: ReadonlyArray<Core.FieldType>): JavaSyntax.ClassBodyDeclaration {
  return (() => {
  const anns = [JavaUtils.overrideAnnotation];
  const mods = [({ tag: "public" })];
  const result = JavaUtils.javaTypeToJavaResult(JavaUtils.javaIntType);
  const returnSum = ({ tag: "statement", value: LibLogic.ifElse(LibLists.null_(fields))(JavaUtils.javaReturnStatement(JavaUtils.javaIntExpression(0n)))(JavaUtils.javaReturnStatement(JavaUtils.javaAdditiveExpressionToJavaExpression(JavaUtils.addExpressions(LibLists.zipWith(hashCodeMultPair)(first20Primes)(LibLists.map(((f: Core.FieldType) => ((_x) => _x.name)(f)))(fields)))))) });
  return JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.hashCodeMethodName)([])(result)([returnSum]);
})();
}

export function recordMemberVar<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.FieldType) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclaration))) {
  return ((ft: Core.FieldType) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const mods = [({ tag: "public" }), ({ tag: "final" })];
  const fname = ((_x) => _x.name)(ft);
  const ftype = ((_x) => _x.type)(ft);
  return LibEithers.bind(encodeType(aliases)(LibSets.empty)(ftype)(cx)(g))(((jt: JavaSyntax.Type) => ({ tag: "right", value: JavaUtils.javaMemberField(mods)(jt)(JavaUtils.fieldNameToJavaVariableDeclarator(fname)) })));
})())));
}

export function recordWithMethod<t0>(aliases: JavaEnvironment.Aliases): ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => ((x: Core.FieldType) => ((x: t0) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclaration))))) {
  return ((elName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => ((field: Core.FieldType) => ((cx: t0) => ((g: Graph.Graph) => (() => {
  const mods = [({ tag: "public" })];
  const anns = [];
  const methodName = LibStrings.cat2("with")(Formatting.nonAlnumToUnderscores(Formatting.capitalize(((_x) => _x)(((_x) => _x.name)(field)))));
  const result = JavaUtils.referenceTypeToResult(JavaUtils.nameToJavaReferenceType(aliases)(false)([])(elName)(null));
  const consId = JavaUtils.sanitizeJavaName(Names.localNameOf(elName));
  const fieldArgs = LibLists.map(((f: Core.FieldType) => JavaUtils.fieldNameToJavaExpression(((_x) => _x.name)(f))))(fields);
  const returnStmt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName(consId)(null))(fieldArgs)(null)) });
  return LibEithers.bind(fieldTypeToFormalParam(aliases)(field)(cx)(g))(((param: JavaSyntax.FormalParameter) => ({ tag: "right", value: JavaUtils.methodDeclaration(mods)([])(anns)(methodName)([param])(result)([returnStmt]) })));
})())))));
}

export function resolveTypeApps(schemeVars: ReadonlyArray<Core.Name>): ((x: ReadonlyArray<Core.Type>) => ((x: ReadonlyMap<Core.Name, Core.Type>) => ReadonlyArray<Core.Type>)) {
  return ((fallbackTypeApps: ReadonlyArray<Core.Type>) => ((argSubst: ReadonlyMap<Core.Name, Core.Type>) => (() => {
  const resolvedVars = LibSets.fromList(LibMaps.keys(argSubst));
  return (() => {
  const unresolvedVars = LibLists.filter(((v: Core.Name) => LibLogic.not(LibSets.member(v)(resolvedVars))))(schemeVars);
  return (() => {
  const usedTypes = LibSets.fromList(LibMaps.elems(argSubst));
  return (() => {
  const unusedIrTypes = LibLists.filter(((t: Core.Type) => LibLogic.not(LibSets.member(t)(usedTypes))))(fallbackTypeApps);
  return (() => {
  const remainingSubst = LibMaps.fromList(LibLists.zip(unresolvedVars)(unusedIrTypes));
  return (() => {
  const fullSubst = LibMaps.union(argSubst)(remainingSubst);
  return LibLists.map(((v: Core.Name) => LibMaps.findWithDefault(({ tag: "variable", value: v }))(v)(fullSubst)))(schemeVars);
})();
})();
})();
})();
})();
})()));
}

export function selfRefSubstitution<t0>(grouped: ReadonlyMap<t0, ReadonlyArray<t0>>): ReadonlyMap<t0, t0> {
  return LibLists.foldl(((subst: ReadonlyMap<t0, t0>) => ((entry: readonly [t0, ReadonlyArray<t0>]) => selfRefSubstitution_processGroup(subst)(LibPairs.first(entry))(LibPairs.second(entry)))))(LibMaps.empty)(LibMaps.toList(grouped));
}

export function selfRefSubstitution_processGroup<t0>(subst: ReadonlyMap<t0, t0>): ((x: t0) => ((x: ReadonlyArray<t0>) => ReadonlyMap<t0, t0>)) {
  return ((inVar: t0) => ((outVars: ReadonlyArray<t0>) => LibLogic.ifElse(LibLists.elem(inVar)(outVars))(LibLists.foldl(((s: ReadonlyMap<t0, t0>) => ((v: t0) => LibLogic.ifElse(LibEquality.equal(v)(inVar))(s)(LibMaps.insert(v)(inVar)(s)))))(subst)(outVars))(subst)));
}

export function serializableTypes(isSer: boolean): ReadonlyArray<JavaSyntax.InterfaceType> {
  return (() => {
  const javaSerializableType = ({
    annotations: [],
    qualifier: ({ tag: "none" }),
    identifier: JavaUtils.javaTypeIdentifier("Serializable"),
    arguments: []
  });
  return LibLogic.ifElse(isSer)([javaSerializableType])([]);
})();
}

export function splitConstantInitializer(member: JavaSyntax.InterfaceMemberDeclaration): ReadonlyArray<JavaSyntax.InterfaceMemberDeclaration> {
  return (() => {
  const _m = member;
  switch (_m.tag) {
    case "constant": return ((cd: JavaSyntax.ConstantDeclaration) => LibLists.bind(((_x) => _x.variables)(cd))(((v1: JavaSyntax.VariableDeclarator) => splitConstantInitializer_splitVar(((_x) => _x.modifiers)(cd))(((_x) => _x.type)(cd))(v1))))((_m as any).value);
    default: return [member](_m);
  }
})();
}

export function splitConstantInitializer_splitVar(mods: ReadonlyArray<JavaSyntax.ConstantModifier>): ((x: JavaSyntax.UnannType) => ((x: JavaSyntax.VariableDeclarator) => ReadonlyArray<JavaSyntax.InterfaceMemberDeclaration>)) {
  return ((utype: JavaSyntax.UnannType) => ((vd: JavaSyntax.VariableDeclarator) => (() => {
  const vid = ((_x) => _x.id)(vd);
  const mInit = ((_x) => _x.initializer)(vd);
  return LibMaybes.cases(mInit)([({ tag: "constant", value: ({
    modifiers: mods,
    type: utype,
    variables: [vd]
  }) })])(((init_: JavaSyntax.VariableInitializer) => (() => {
  const _m = init_;
  switch (_m.tag) {
    case "expression": return ((expr: JavaSyntax.Expression) => (() => {
  const varName = javaIdentifierToString(((_x) => _x.identifier)(vid));
  const helperName = LibStrings.cat2("_init_")(varName);
  const callExpr = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocation(null)(helperName)([]));
  const field = ({ tag: "constant", value: ({
    modifiers: mods,
    type: utype,
    variables: [({
    id: vid,
    initializer: ({ tag: "expression", value: callExpr })
  })]
  }) });
  const returnSt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(expr) });
  const resultType = ({ tag: "type", value: utype });
  const helper = JavaUtils.interfaceMethodDeclaration([({ tag: "static" }), ({ tag: "private" })])([])(helperName)([])(resultType)([returnSt]);
  return [field, helper];
})())((_m as any).value);
    default: return [({ tag: "constant", value: ({
    modifiers: mods,
    type: utype,
    variables: [vd]
  }) })](_m);
  }
})()));
})()));
}

export function stripForalls(t: Core.Type): Core.Type {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "forall": return ((fa: Core.ForallType) => stripForalls(((_x) => _x.body)(fa)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function substituteTypeVarsWithTypes(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => substituteTypeVarsWithTypes_go(subst)(Strip.deannotateType(t)));
}

export function substituteTypeVarsWithTypes_go(subst: ReadonlyMap<Core.Name, Core.Type>): ((x: Core.Type) => Core.Type) {
  return ((t: Core.Type) => (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "variable": return ((v: Core.Name) => LibMaybes.cases(LibMaps.lookup(v)(subst))(t)(((rep: Core.Type) => rep)))((_m as any).value);
    case "function": return ((ft: Core.FunctionType) => ({ tag: "function", value: ({
    domain: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.domain)(ft)),
    codomain: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.codomain)(ft))
  }) }))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => ({ tag: "application", value: ({
    function: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.function)(at)),
    argument: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.argument)(at))
  }) }))((_m as any).value);
    case "list": return ((inner: Core.Type) => ({ tag: "list", value: substituteTypeVarsWithTypes_go(subst)(inner) }))((_m as any).value);
    case "set": return ((inner: Core.Type) => ({ tag: "set", value: substituteTypeVarsWithTypes_go(subst)(inner) }))((_m as any).value);
    case "maybe": return ((inner: Core.Type) => ({ tag: "maybe", value: substituteTypeVarsWithTypes_go(subst)(inner) }))((_m as any).value);
    case "map": return ((mt: Core.MapType) => ({ tag: "map", value: ({
    keys: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.keys)(mt)),
    values: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.values)(mt))
  }) }))((_m as any).value);
    case "pair": return ((pt: Core.PairType) => ({ tag: "pair", value: ({
    first: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.first)(pt)),
    second: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.second)(pt))
  }) }))((_m as any).value);
    case "either": return ((et: Core.EitherType) => ({ tag: "either", value: ({
    left: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.left)(et)),
    right: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.right)(et))
  }) }))((_m as any).value);
    case "forall": return ((ft: Core.ForallType) => ({ tag: "forall", value: ({
    parameter: ((_x) => _x.parameter)(ft),
    body: substituteTypeVarsWithTypes_go(subst)(((_x) => _x.body)(ft))
  }) }))((_m as any).value);
    default: return t(_m);
  }
})());
}

export const tagCmpNotZeroExpr: JavaSyntax.Expression = (() => {
  const lhs = JavaUtils.javaRelationalExpressionToJavaEqualityExpression(JavaUtils.javaPostfixExpressionToJavaRelationalExpression(({ tag: "name", value: ({
    qualifier: null,
    identifier: JavaUtils.javaIdentifier("tagCmp")
  }) })));
  const rhs = JavaUtils.javaPostfixExpressionToJavaRelationalExpression(({ tag: "primary", value: JavaUtils.javaLiteralToJavaPrimary(JavaUtils.javaInt(0n)) }));
  return JavaUtils.javaEqualityExpressionToJavaExpression(({ tag: "notEqual", value: ({
    lhs: lhs,
    rhs: rhs
  }) }));
})();

export const tagCompareExpr: JavaSyntax.Expression = (() => {
  const thisGetClass = ({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "primary", value: JavaUtils.javaExpressionToJavaPrimary(JavaUtils.javaThis) }),
    typeArguments: [],
    identifier: "getClass"
  }) }),
    arguments: []
  });
  const thisGetName = ({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "primary", value: JavaUtils.javaMethodInvocationToJavaPrimary(thisGetClass) }),
    typeArguments: [],
    identifier: "getName"
  }) }),
    arguments: []
  });
  const otherGetClass = ({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "expression", value: ({
    qualifier: null,
    identifier: JavaNames.otherInstanceName
  }) }),
    typeArguments: [],
    identifier: "getClass"
  }) }),
    arguments: []
  });
  const otherGetName = ({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "primary", value: JavaUtils.javaMethodInvocationToJavaPrimary(otherGetClass) }),
    typeArguments: [],
    identifier: "getName"
  }) }),
    arguments: []
  });
  return JavaUtils.javaMethodInvocationToJavaExpression(({
    header: ({ tag: "complex", value: ({
    variant: ({ tag: "primary", value: JavaUtils.javaMethodInvocationToJavaPrimary(thisGetName) }),
    typeArguments: [],
    identifier: JavaNames.compareToMethodName
  }) }),
    arguments: [JavaUtils.javaMethodInvocationToJavaExpression(otherGetName)]
  }));
})();

export function takeTypeArgs<t0, t1>(label: string): ((x: number) => ((x: ReadonlyArray<JavaSyntax.Type>) => ((x: t0) => ((x: t1) => Errors.Error | ReadonlyArray<JavaSyntax.TypeArgument>)))) {
  return ((n: number) => ((tyapps: ReadonlyArray<JavaSyntax.Type>) => ((cx: t0) => ((g: t1) => LibLogic.ifElse(LibEquality.lt(LibLists.length(tyapps))(n))(({ tag: "left", value: ({ tag: "other", value: LibStrings.cat(["needed type arguments for ", label, ", found too few"]) }) }))(LibEithers.mapList(((jt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: ({ tag: "reference", value: rt }) })))))(LibLists.take(n)(tyapps)))))));
}

export function toClassDecl(isInner: boolean): ((x: boolean) => ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<JavaSyntax.TypeParameter>) => ((x: Core.Name) => ((x: Core.Type) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassDeclaration))))))) {
  return ((isSer: boolean) => ((aliases: JavaEnvironment.Aliases) => ((tparams: ReadonlyArray<JavaSyntax.TypeParameter>) => ((elName: Core.Name) => ((t: Core.Type) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const wrap = ((t_: Core.Type) => declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)([({
    name: "value",
    type: Strip.deannotateType(t_)
  })])(cx)(g));
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "record": return ((rt: ReadonlyArray<Core.FieldType>) => declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)(rt)(cx)(g))((_m as any).value);
    case "union": return ((rt: ReadonlyArray<Core.FieldType>) => declarationForUnionType(isSer)(aliases)(tparams)(elName)(rt)(cx)(g))((_m as any).value);
    case "forall": return ((fa: Core.ForallType) => (() => {
  const v = ((_x) => _x.parameter)(fa);
  return (() => {
  const body = ((_x) => _x.body)(fa);
  return (() => {
  const param = JavaUtils.javaTypeParameter(Formatting.capitalize(((_x) => _x)(v)));
  return toClassDecl(false)(isSer)(aliases)(LibLists.concat2(tparams)([param]))(elName)(body)(cx)(g);
})();
})();
})())((_m as any).value);
    case "wrap": return ((wt: Core.Type) => declarationForRecordType(isInner)(isSer)(aliases)(tparams)(elName)([({
    name: "value",
    type: wt
  })])(cx)(g))((_m as any).value);
    default: return wrap(t)(_m);
  }
})();
})())))))));
}

export function toDeclInit(aliasesExt: JavaEnvironment.Aliases): ((x: Graph.Graph) => ((x: ReadonlySet<Core.Name>) => ((x: ReadonlyArray<Core.Binding>) => ((x: Core.Name) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.BlockStatement | null)))))) {
  return ((gExt: Graph.Graph) => ((recursiveVars: ReadonlySet<Core.Name>) => ((flatBindings: ReadonlyArray<Core.Binding>) => ((name: Core.Name) => ((cx: Context.Context) => ((g: Graph.Graph) => LibLogic.ifElse(LibSets.member(name)(recursiveVars))((() => {
  const binding = LibLists.head(LibLists.filter(((b: Core.Binding) => LibEquality.equal(((_x) => _x.name)(b))(name)))(flatBindings));
  return (() => {
  const value = ((_x) => _x.term)(binding);
  return LibEithers.bind(LibMaybes.cases(((_x) => _x.type)(binding))(Checking.typeOfTerm(cx)(gExt)(value))(((ts: Core.TypeScheme) => ({ tag: "right", value: ((_x) => _x.type)(ts) }))))(((typ: Core.Type) => LibEithers.bind(encodeType(aliasesExt)(LibSets.empty)(typ)(cx)(g))(((jtype: JavaSyntax.Type) => (() => {
  const id = JavaUtils.variableToJavaIdentifier(name);
  return (() => {
  const arid = "java.util.concurrent.atomic.AtomicReference";
  return (() => {
  const aid = ({
    annotations: [],
    identifier: arid
  });
  return LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => (() => {
  const targs = typeArgsOrDiamond([({ tag: "reference", value: rt })]);
  return (() => {
  const ci = ({
    identifiers: [aid],
    typeArguments: targs
  });
  return (() => {
  const body = JavaUtils.javaConstructorCall(ci)([])(null);
  return (() => {
  const pkg = JavaNames.javaPackageName(["java", "util", "concurrent", "atomic"]);
  return (() => {
  const artype = JavaUtils.javaRefType([rt])(pkg)("AtomicReference");
  return ({ tag: "right", value: JavaUtils.variableDeclarationStatement(aliasesExt)(artype)(id)(body) });
})();
})();
})();
})();
})()));
})();
})();
})()))));
})();
})())(({ tag: "right", value: null }))))))));
}

export function toDeclStatement(envExt: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: Graph.Graph) => ((x: ReadonlySet<Core.Name>) => ((x: ReadonlySet<Core.Name>) => ((x: ReadonlyArray<Core.Binding>) => ((x: Core.Name) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.BlockStatement)))))))) {
  return ((aliasesExt: JavaEnvironment.Aliases) => ((gExt: Graph.Graph) => ((recursiveVars: ReadonlySet<Core.Name>) => ((thunkedVars: ReadonlySet<Core.Name>) => ((flatBindings: ReadonlyArray<Core.Binding>) => ((name: Core.Name) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const binding = LibLists.head(LibLists.filter(((b: Core.Binding) => LibEquality.equal(((_x) => _x.name)(b))(name)))(flatBindings));
  return (() => {
  const value = ((_x) => _x.term)(binding);
  return LibEithers.bind(LibMaybes.cases(((_x) => _x.type)(binding))(Checking.typeOfTerm(cx)(gExt)(value))(((ts: Core.TypeScheme) => ({ tag: "right", value: ((_x) => _x.type)(ts) }))))(((typ: Core.Type) => LibEithers.bind(encodeType(aliasesExt)(LibSets.empty)(typ)(cx)(g))(((jtype: JavaSyntax.Type) => (() => {
  const id = JavaUtils.variableToJavaIdentifier(name);
  return (() => {
  const annotatedValue = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(typ))(value);
  return LibEithers.bind(encodeTerm(envExt)(annotatedValue)(cx)(g))(((rhs: JavaSyntax.Expression) => LibLogic.ifElse(LibSets.member(name)(recursiveVars))(({ tag: "right", value: ({ tag: "statement", value: JavaUtils.javaMethodInvocationToJavaStatement(JavaUtils.methodInvocation(({ tag: "left", value: ({
    qualifier: null,
    identifier: id
  }) }))(JavaNames.setMethodName)([rhs])) }) }))(LibLogic.ifElse(LibSets.member(name)(thunkedVars))(LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => (() => {
  const lazyType = JavaUtils.javaRefType([rt])(JavaNames.hydraUtilPackageName)("Lazy");
  return (() => {
  const lambdaBody = ({ tag: "expression", value: rhs });
  return (() => {
  const supplierLambda = ({ tag: "lambda", value: ({
    parameters: ({ tag: "tuple", value: [] }),
    body: lambdaBody
  }) });
  return (() => {
  const targs = typeArgsOrDiamond([({ tag: "reference", value: rt })]);
  return (() => {
  const lazyExpr = JavaUtils.javaConstructorCall(JavaUtils.javaConstructorName("hydra.util.Lazy")(targs))([supplierLambda])(null);
  return ({ tag: "right", value: JavaUtils.variableDeclarationStatement(aliasesExt)(lazyType)(id)(lazyExpr) });
})();
})();
})();
})();
})())))(({ tag: "right", value: JavaUtils.variableDeclarationStatement(aliasesExt)(jtype)(id)(rhs) })))));
})();
})()))));
})();
})()))))))));
}

export function tryInferFunctionType(funTerm: Core.Term): Core.Type | null {
  return (() => {
  const _m = Strip.deannotateTerm(funTerm);
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => LibMaybes.bind(((_x) => _x.domain)(lam))(((dom: Core.Type) => (() => {
  const mCod = (() => {
  const _m = ((_x) => _x.body)(lam);
  switch (_m.tag) {
    case "annotated": return ((at: Core.AnnotatedTerm) => LibMaybes.bind(LibMaps.lookup(Constants.key_type)(((_x) => _x.annotation)(at)))(((typeTerm: Core.Term) => decodeTypeFromTerm(typeTerm))))((_m as any).value);
    case "lambda": return ((_innerLam: Core.Lambda) => tryInferFunctionType(((_x) => _x.body)(lam)))((_m as any).value);
    default: return null(_m);
  }
})();
  return LibMaybes.map(((cod: Core.Type) => ({ tag: "function", value: ({
    domain: dom,
    codomain: cod
  }) })))(mCod);
})())))((_m as any).value);
    default: return null(_m);
  }
})();
}

export function typeAppFallbackCast(env: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((x: ReadonlyArray<JavaSyntax.Type>) => ((x: JavaSyntax.Type) => ((x: Core.Term) => ((x: Core.Type) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression)))))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((anns: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((tyapps: ReadonlyArray<JavaSyntax.Type>) => ((jatyp: JavaSyntax.Type) => ((body: Core.Term) => ((typ: Core.Type) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const annotatedBody = Annotations.setTermAnnotation(Constants.key_type)(EncodeCore.type(typ))(body);
  return LibEithers.bind(encodeTermInternal(env)(anns)(LibLists.cons(jatyp)(tyapps))(annotatedBody)(cx)(g))(((jbody: JavaSyntax.Expression) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(typ)(cx)(g))(((jtype: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jtype)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(rt)(JavaUtils.javaExpressionToJavaUnaryExpression(jbody))) })))))));
})()))))))));
}

export function typeAppNullaryOrHoisted(env: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((x: ReadonlyArray<JavaSyntax.Type>) => ((x: JavaSyntax.Type) => ((x: Core.Term) => ((x: Core.Type) => ((x: Core.Name) => ((x: JavaEnvironment.JavaSymbolClass) => ((x: ReadonlyArray<Core.Type>) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.Expression))))))))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((anns: ReadonlyArray<ReadonlyMap<Core.Name, Core.Term>>) => ((tyapps: ReadonlyArray<JavaSyntax.Type>) => ((jatyp: JavaSyntax.Type) => ((body: Core.Term) => ((correctedTyp: Core.Type) => ((varName: Core.Name) => ((cls: JavaEnvironment.JavaSymbolClass) => ((allTypeArgs: ReadonlyArray<Core.Type>) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const qn = Names.qualifyName(varName);
  return (() => {
  const mns = ((_x) => _x.namespace)(qn);
  return (() => {
  const localName = ((_x) => _x.local)(qn);
  return (() => {
  const _m = cls;
  switch (_m.tag) {
    case "nullaryFunction": return ((_u: void) => LibMaybes.cases(mns)(typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))(((ns_: Packaging.Namespace) => (() => {
  const classId = JavaUtils.nameToJavaName(aliases)(elementsQualifiedName(ns_));
  return (() => {
  const methodId = JavaUtils.sanitizeJavaName(localName);
  return LibEithers.bind(filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))(((filteredTypeArgs: ReadonlyArray<Core.Type>) => LibEithers.bind(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: ({ tag: "reference", value: rt }) })))))))(filteredTypeArgs))(((jTypeArgs: ReadonlyArray<JavaSyntax.TypeArgument>) => ({ tag: "right", value: JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)([])) })))));
})();
})())))((_m as any).value);
    case "hoistedLambda": return ((arity: number) => LibMaybes.cases(mns)(typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g))(((ns_: Packaging.Namespace) => (() => {
  const classId = JavaUtils.nameToJavaName(aliases)(elementsQualifiedName(ns_));
  return (() => {
  const methodId = JavaUtils.sanitizeJavaName(localName);
  return LibEithers.bind(filterPhantomTypeArgs(varName)(allTypeArgs)(cx)(g))(((filteredTypeArgs: ReadonlyArray<Core.Type>) => LibEithers.bind(LibEithers.mapList(((t: Core.Type) => LibEithers.bind(encodeType(aliases)(LibSets.empty)(t)(cx)(g))(((jt: JavaSyntax.Type) => LibEithers.bind(JavaUtils.javaTypeToJavaReferenceType(jt)(cx))(((rt: JavaSyntax.ReferenceType) => ({ tag: "right", value: ({ tag: "reference", value: rt }) })))))))(filteredTypeArgs))(((jTypeArgs: ReadonlyArray<JavaSyntax.TypeArgument>) => (() => {
  const paramNames = LibLists.map(((i: number) => LibStrings.cat2("p")(LibLiterals.showInt32(i))))(LibMath.range(0)(LibMath.sub(arity)(1)));
  return (() => {
  const paramExprs = LibLists.map(((p: Core.Name) => JavaUtils.javaIdentifierToJavaExpression(JavaUtils.variableToJavaIdentifier(p))))(paramNames);
  return (() => {
  const call = JavaUtils.javaMethodInvocationToJavaExpression(JavaUtils.methodInvocationStaticWithTypeArgs(classId)(methodId)(jTypeArgs)(paramExprs));
  return ({ tag: "right", value: buildCurriedLambda(paramNames)(call) });
})();
})();
})()))));
})();
})())))((_m as any).value);
    default: return typeAppFallbackCast(env)(aliases)(anns)(tyapps)(jatyp)(body)(correctedTyp)(cx)(g)(_m);
  }
})();
})();
})();
})())))))))))));
}

export function typeArgsOrDiamond(args: ReadonlyArray<JavaSyntax.TypeArgument>): JavaSyntax.TypeArgumentsOrDiamond {
  return LibLogic.ifElse(((_x) => _x.supportsDiamondOperator)(javaFeatures))(({ tag: "diamond" }))(({ tag: "arguments", value: args }));
}

export function typesMatch(a: Core.Type): ((x: Core.Type) => boolean) {
  return ((b: Core.Type) => (() => {
  const _m = a;
  switch (_m.tag) {
    case "variable": return ((va: Core.Name) => (() => {
  const _m = b;
  switch (_m.tag) {
    case "variable": return ((vb: Core.Name) => LibEquality.equal(va)(vb))((_m as any).value);
    default: return true(_m);
  }
})())((_m as any).value);
    case "wrap": return ((wa: Core.Type) => (() => {
  const _m = b;
  switch (_m.tag) {
    case "wrap": return ((wb: Core.Type) => LibEquality.equal(wa)(wb))((_m as any).value);
    default: return true(_m);
  }
})())((_m as any).value);
    default: return true(_m);
  }
})());
}

export function unwrapReturnType(t: Core.Type): Core.Type {
  return (() => {
  const _m = Strip.deannotateType(t);
  switch (_m.tag) {
    case "function": return ((ft: Core.FunctionType) => unwrapReturnType(((_x) => _x.codomain)(ft)))((_m as any).value);
    case "application": return ((at: Core.ApplicationType) => unwrapReturnType(((_x) => _x.argument)(at)))((_m as any).value);
    default: return t(_m);
  }
})();
}

export function variantCompareToMethod<t0>(aliases: JavaEnvironment.Aliases): ((x: t0) => ((x: Core.Name) => ((x: Core.Name) => ((x: ReadonlyArray<Core.FieldType>) => JavaSyntax.ClassBodyDeclaration)))) {
  return ((tparams: t0) => ((parentName: Core.Name) => ((variantName: Core.Name) => ((fields: ReadonlyArray<Core.FieldType>) => (() => {
  const anns = [JavaUtils.overrideAnnotation, JavaUtils.suppressWarningsUncheckedAnnotation];
  const mods = [({ tag: "public" })];
  const param = JavaUtils.javaTypeToJavaFormalParameter(JavaUtils.javaTypeFromTypeName(aliases)(parentName))(JavaNames.otherInstanceName);
  const result = JavaUtils.javaTypeToJavaResult(JavaUtils.javaIntType);
  const varTmpName = "o";
  const tagDeclStmt = JavaUtils.variableDeclarationStatement(aliases)(JavaUtils.javaIntType)(JavaUtils.javaIdentifier("tagCmp"))(tagCompareExpr);
  const tagReturnStmt = ({ tag: "statement", value: ({ tag: "ifThen", value: ({
    expression: tagCmpNotZeroExpr,
    statement: JavaUtils.javaReturnStatement(JavaUtils.javaExpressionNameToJavaExpression(({
    qualifier: null,
    identifier: JavaUtils.javaIdentifier("tagCmp")
  })))
  }) }) });
  const variantJavaType = JavaUtils.javaTypeFromTypeName(aliases)(variantName);
  const castOtherExpr = JavaUtils.javaCastExpressionToJavaExpression(JavaUtils.javaCastExpression(JavaUtils.nameToJavaReferenceType(aliases)(false)([])(variantName)(null))(JavaUtils.javaIdentifierToJavaUnaryExpression(JavaNames.otherInstanceName)));
  const castDeclStmt = JavaUtils.variableDeclarationStatement(aliases)(variantJavaType)(JavaUtils.javaIdentifier(varTmpName))(castOtherExpr);
  const emptyReturn = [({ tag: "statement", value: JavaUtils.javaReturnStatement(JavaUtils.javaIntExpression(0n)) })];
  const valueCompareStmt = LibLogic.ifElse(LibLists.null_(fields))(emptyReturn)(LibLists.concat2([castDeclStmt])(compareToBody(aliases)(varTmpName)(fields)));
  const body = LibLists.concat2([tagDeclStmt, tagReturnStmt])(valueCompareStmt);
  return JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.compareToMethodName)([param])(result)(body);
})()))));
}

export function visitBranch(env: JavaEnvironment.JavaEnvironment): ((x: JavaEnvironment.Aliases) => ((x: Core.Type) => ((x: Core.Name) => ((x: JavaSyntax.Type) => ((x: ReadonlyArray<JavaSyntax.TypeArgument>) => ((x: Core.Field) => ((x: Context.Context) => ((x: Graph.Graph) => Errors.Error | JavaSyntax.ClassBodyDeclarationWithComments)))))))) {
  return ((aliases: JavaEnvironment.Aliases) => ((dom: Core.Type) => ((tname: Core.Name) => ((jcod: JavaSyntax.Type) => ((targs: ReadonlyArray<JavaSyntax.TypeArgument>) => ((field: Core.Field) => ((cx: Context.Context) => ((g: Graph.Graph) => (() => {
  const jdom = ({ tag: "reference", value: JavaUtils.nameToJavaReferenceType(aliases)(true)(targs)(tname)(Formatting.capitalize(((_x) => _x)(((_x) => _x.name)(field)))) });
  return (() => {
  const mods = [({ tag: "public" })];
  return (() => {
  const anns = [JavaUtils.overrideAnnotation];
  return (() => {
  const result = ({ tag: "type", value: jcod });
  return (() => {
  const _m = Strip.deannotateTerm(((_x) => _x.term)(field));
  switch (_m.tag) {
    case "lambda": return ((lam: Core.Lambda) => withLambda(env)(lam)(((env2: JavaEnvironment.JavaEnvironment) => (() => {
  const lambdaParam = ((_x) => _x.parameter)(lam);
  return (() => {
  const body = ((_x) => _x.body)(lam);
  return (() => {
  const env3 = insertBranchVar(lambdaParam)(env2);
  return LibEithers.bind(analyzeJavaFunction(env3)(body)(cx)(g))(((fs: Typing.FunctionStructure<JavaEnvironment.JavaEnvironment>) => (() => {
  const bindings = ((_x) => _x.bindings)(fs);
  return (() => {
  const innerBody = ((_x) => _x.body)(fs);
  return (() => {
  const env4 = ((_x) => _x.environment)(fs);
  return LibEithers.bind(bindingsToStatements(env4)(bindings)(cx)(g))(((bindResult: readonly [ReadonlyArray<JavaSyntax.BlockStatement>, JavaEnvironment.JavaEnvironment]) => (() => {
  const bindingStmts = LibPairs.first(bindResult);
  return (() => {
  const env5 = LibPairs.second(bindResult);
  return LibEithers.bind(encodeTerm(env5)(innerBody)(cx)(g))(((jret: JavaSyntax.Expression) => (() => {
  const param = JavaUtils.javaTypeToJavaFormalParameter(jdom)(lambdaParam);
  return (() => {
  const returnStmt = ({ tag: "statement", value: JavaUtils.javaReturnStatement(jret) });
  return (() => {
  const allStmts = LibLists.concat2(bindingStmts)([returnStmt]);
  return ({ tag: "right", value: noComment(JavaUtils.methodDeclaration(mods)([])(anns)(JavaNames.visitMethodName)([param])(result)(allStmts)) });
})();
})();
})()));
})();
})()));
})();
})();
})()));
})();
})();
})())))((_m as any).value);
    default: return ({ tag: "left", value: ({ tag: "other", value: LibStrings.cat2("visitBranch: field term is not a lambda: ")(ShowCore.term(((_x) => _x.term)(field))) }) })(_m);
  }
})();
})();
})();
})();
})()))))))));
}

export function withLambda<t0>(env: JavaEnvironment.JavaEnvironment): ((x: Core.Lambda) => ((x: ((x: JavaEnvironment.JavaEnvironment) => t0)) => t0)) {
  return ((lam: Core.Lambda) => ((k: ((x: JavaEnvironment.JavaEnvironment) => t0)) => Environment.withLambdaContext(javaEnvGetGraph)(javaEnvSetGraph)(env)(lam)(((env1: JavaEnvironment.JavaEnvironment) => (() => {
  const aliases = ((_x) => _x.aliases)(env1);
  return (() => {
  const aliases2 = ({
    currentNamespace: ((_x) => _x.currentNamespace)(aliases),
    packages: ((_x) => _x.packages)(aliases),
    branchVars: ((_x) => _x.branchVars)(aliases),
    recursiveVars: ((_x) => _x.recursiveVars)(aliases),
    inScopeTypeParams: ((_x) => _x.inScopeTypeParams)(aliases),
    polymorphicLocals: ((_x) => _x.polymorphicLocals)(aliases),
    inScopeJavaVars: ((_x) => _x.inScopeJavaVars)(aliases),
    varRenames: ((_x) => _x.varRenames)(aliases),
    lambdaVars: LibSets.insert(((_x) => _x.parameter)(lam))(((_x) => _x.lambdaVars)(aliases)),
    typeVarSubst: ((_x) => _x.typeVarSubst)(aliases),
    trustedTypeVars: ((_x) => _x.trustedTypeVars)(aliases),
    methodCodomain: ((_x) => _x.methodCodomain)(aliases),
    thunkedVars: ((_x) => _x.thunkedVars)(aliases)
  });
  return (() => {
  const env2 = ({
    aliases: aliases2,
    graph: ((_x) => _x.graph)(env1)
  });
  return k(env2);
})();
})();
})()))));
}

export function withTypeLambda<t0>(v1: JavaEnvironment.JavaEnvironment): ((x: Core.TypeLambda) => ((x: ((x: JavaEnvironment.JavaEnvironment) => t0)) => t0)) {
  return ((v2: Core.TypeLambda) => ((v3: ((x: JavaEnvironment.JavaEnvironment) => t0)) => Environment.withTypeLambdaContext(javaEnvGetGraph)(javaEnvSetGraph)(v1)(v2)(v3)));
}

export function wrapInSupplierLambda(expr: JavaSyntax.Expression): JavaSyntax.Expression {
  return ({ tag: "lambda", value: ({
    parameters: ({ tag: "tuple", value: [] }),
    body: ({ tag: "expression", value: expr })
  }) });
}

export function wrapLazyArguments(name: Core.Name): ((x: ReadonlyArray<JavaSyntax.Expression>) => readonly [ReadonlyArray<JavaSyntax.Expression>, string | null]) {
  return ((args: ReadonlyArray<JavaSyntax.Expression>) => LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.logic.ifElse"))(LibEquality.equal(LibLists.length(args))(3)))([[LibLists.at(0)(args), wrapInSupplierLambda(LibLists.at(1)(args)), wrapInSupplierLambda(LibLists.at(2)(args))], "lazy"])(LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.maybes.maybe"))(LibEquality.equal(LibLists.length(args))(3)))([[wrapInSupplierLambda(LibLists.at(0)(args)), LibLists.at(1)(args), LibLists.at(2)(args)], "applyLazy"])(LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.maybes.cases"))(LibEquality.equal(LibLists.length(args))(3)))([[LibLists.at(0)(args), wrapInSupplierLambda(LibLists.at(1)(args)), LibLists.at(2)(args)], "applyLazy"])(LibLogic.ifElse(LibLogic.and(LibEquality.equal(name)("hydra.lib.maps.findWithDefault"))(LibEquality.equal(LibLists.length(args))(3)))([[wrapInSupplierLambda(LibLists.at(0)(args)), LibLists.at(1)(args), LibLists.at(2)(args)], "applyLazy"])(LibLogic.ifElse(LibLogic.and(LibLogic.or(LibEquality.equal(name)("hydra.lib.maybes.fromMaybe"))(LibLogic.or(LibEquality.equal(name)("hydra.lib.eithers.fromLeft"))(LibEquality.equal(name)("hydra.lib.eithers.fromRight"))))(LibEquality.equal(LibLists.length(args))(2)))([[wrapInSupplierLambda(LibLists.at(0)(args)), LibLists.at(1)(args)], "applyLazy"])([args, null]))))));
}
